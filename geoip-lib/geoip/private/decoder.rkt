#lang racket/base

(require racket/bytes
         racket/contract/base
         racket/function
         racket/match
         net/ip)

(provide (contract-out
          [geoip? (-> any/c boolean?)]
          [geoip-lookup (-> geoip? string? (or/c false/c hash?))]
          [make-geoip (-> path-string? geoip?)]))


;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-geoip path)
  (define buffer (make-bytes 16384))
  (call-with-input-file path
    (lambda (in)
      (let loop ([chunks '()])
        (match (read-bytes-avail!/enable-break buffer in)
          [(? eof-object?)
           (define bs (bytes->immutable-bytes (bytes-join (reverse chunks) #"")))

           (define metadata-marker (bytes-find-last bs METADATA-MARKER))
           (unless metadata-marker
             (error 'make-geoip "could not find metadata marker in database file"))

           (define-values (_ metadata)
             (decode-field bs (+ (bytes-length METADATA-MARKER) metadata-marker)))

           (define format-version (hash-ref metadata "binary_format_major_version"))
           (unless (= format-version 2)
             (error 'make-geoip (format "database version ~a not supported" format-version)))

           (define record-size (hash-ref metadata "record_size"))
           (define node-count (hash-ref metadata "node_count"))
           (define tree-size (* (/ (* record-size 2) 8) node-count))
           (define tree (subbytes bs 0 tree-size))
           (define data (subbytes bs (+ 16 tree-size) metadata-marker))

           (geoip tree data metadata node-count record-size)]

          [n-bytes-read
           (define chunk (subbytes buffer 0 n-bytes-read))
           (loop (cons chunk chunks))])))))

(define (geoip-lookup a-geoip ip)
  (define node-count (geoip-node-count a-geoip))
  (define ip-address (string->geoip-ip-address a-geoip ip))

  (let loop ([bytes (ip-address->bytes ip-address)]
             [bits null]
             [index 0])
    (cond
      [(equal? bytes #"")   #f]
      [(= index node-count) #f]

      [(null? bits)
       (loop (subbytes bytes 1) (byte->bitlist (bytes-ref bytes 0)) index)]

      [else
       (define-values (lhs rhs)
         (geoip-tree-ref a-geoip index))

       (define index*
         (if (= (car bits) 0)
             lhs
             rhs))

       (if (> index* node-count)
           (geoip-data-ref a-geoip index*)
           (loop bytes (cdr bits) index*))])))


;; Private API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define METADATA-MARKER #"\xAB\xCD\xEFMaxMind.com")

(struct geoip (tree data metadata node-count record-size))

(define (string->geoip-ip-address a-geoip ip)
  (define ip-address (make-ip-address ip))
  (define ip-version (hash-ref (geoip-metadata a-geoip) "ip_version"))
  (match (list ip-version (ip-address-version ip-address))
    [(list 4 6) (raise-argument-error 'string->geoip-ip-address "IPv4 database does not support IPv6 addresses" ip)]
    [(list 6 4) (ipv4-address->ipv6-address ip-address)]
    [_          ip-address]))

(define (ipv4-address->ipv6-address addr)
  (make-ip-address (bitwise-ior #xFFFF00000000 (ip-address->number addr)) 6))

(define (bytes-find-last bs subbs)
  (define step (bytes-length subbs))
  (for/first ([i (in-range (- (bytes-length bs) step) -1 -1)]
              #:when (equal? subbs (subbytes bs i (+ i step))))
    i))


;;; Tree Lookups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (geoip-tree-ref a-geoip i)
  (match (geoip-record-size a-geoip)
    [24 (decode-node/24 (geoip-tree a-geoip) i)]
    [28 (decode-node/28 (geoip-tree a-geoip) i)]
    [32 (decode-node/32 (geoip-tree a-geoip) i)]))

(define (decode-node/24 tree i)
  (define start-idx (* i 6))
  (values (decode-integer tree start-idx 3)
          (decode-integer tree (+ start-idx 3) 3)))

(define (decode-node/28 tree i)
  (define start-idx (* i 7))
  (define middle (bytes-ref tree (+ start-idx 3)))
  (values
   (bitwise-ior
    (arithmetic-shift (arithmetic-shift middle -4) 24)
    (decode-integer tree start-idx 3))

   (bitwise-ior
    (arithmetic-shift (bitwise-and #b1111 middle) 24)
    (decode-integer tree (+ start-idx 4) 3))))

(define (decode-node/32 tree i)
  (define start-idx (* i 8))
  (values (decode-integer tree start-idx 4)
          (decode-integer tree (+ start-idx 4) 4)))

(define (byte->bitlist b)
  (list (bitwise-and b #b10000000)
        (bitwise-and b #b01000000)
        (bitwise-and b #b00100000)
        (bitwise-and b #b00010000)
        (bitwise-and b #b00001000)
        (bitwise-and b #b00000100)
        (bitwise-and b #b00000010)
        (bitwise-and b #b00000001)))


;;; Data Lookups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (geoip-data-ref a-geoip i)
  (define-values (_ value)
    (decode-field (geoip-data a-geoip) (- i (geoip-node-count a-geoip) 16)))

  value)

(define (decode-field bs [start 0])
  (match (arithmetic-shift (bytes-ref bs start) -5)
    [1
     (let*-values ([(pos pointer) (decode-pointer bs start)]
                   [(_   field)   (decode-field bs pointer)])
       (values pos field))]

    [2 (decode-string     bs start)]
    [3 (decode-double     bs start)]
    [4 (decode-bytes      bs start)]
    [5 (decode-uint/16/32 bs start)]
    [6 (decode-uint/16/32 bs start)]
    [7 (decode-map        bs start)]
    [0
     (match (+ (bytes-ref bs (add1 start)) 7)
       [8  (decode-int32       bs start)]
       [9  (decode-uint/64/128 bs start)]
       [10 (decode-uint/64/128 bs start)]
       [11 (decode-array       bs start)]
       [12 (decode-dcc         bs start)]
       [13 (decode-end-marker  bs start)]
       [14 (decode-boolean     bs start)]
       [15 (decode-float       bs start)])]))

(define (decode-size bs i)
  (define b (bytes-ref bs i))
  (match (bitwise-and #b00011111 b)
    [31 (values (+ 4 i) (+ (decode-integer bs (add1 i) 3) 65821))]
    [30 (values (+ 3 i) (+ (decode-integer bs (add1 i) 2) 285))]
    [29 (values (+ 2 i) (+ (decode-integer bs (add1 i) 1) 29))]
    [s  (values (+ 1 i) s)]))

(define (decode-integer bs start size)
  (for/fold ([n 0])
            ([i (in-range size)])
    (+ n (* (bytes-ref bs (+ start i))
            (expt 256 (sub1 (- size i)))))))

(define (decode-pointer bs start)
  (define b (bytes-ref bs start))
  (define ss  (bitwise-and #b11 (arithmetic-shift b -3)))
  (define vvv (bitwise-and #b111 b))

  (match ss
    [0 (values (+ 2 start)
               (bitwise-ior (arithmetic-shift vvv 8)
                            (bytes-ref bs (add1 start))))]

    [1 (values (+ 3 start)
               (+ 2048
                  (bitwise-ior (arithmetic-shift vvv 16)
                               (arithmetic-shift (bytes-ref bs (add1 start)) 8)
                               (bytes-ref bs (+ 2 start)))))]

    [2 (values (+ 4 start)
               (+ 526336
                  (bitwise-ior (arithmetic-shift vvv 24)
                               (arithmetic-shift (bytes-ref bs (+ 1 start)) 16)
                               (arithmetic-shift (bytes-ref bs (+ 2 start)) 8)
                               (bytes-ref bs (+ 3 start)))))]

    [3 (values (+ 5 start)
               (decode-integer bs (add1 start) 4))]))

(define (decode-string bs start)
  (define-values (data-start size)
    (decode-size bs start))

  (cond
    [(= size 0)
     (values (add1 data-start) "")]

    [else
     (values (+ data-start size)
             (bytes->string/utf-8 bs #f data-start (+ data-start size)))]))

(define (decode-double bs start)
  (values (+ 9 start)
          (floating-point-bytes->real bs #t
                                      (+ 1 start)
                                      (+ 9 start))))

(define (decode-bytes bs start)
  (define-values (data-start size)
    (decode-size bs start))

  (cond
    [(= size 0)
     (values (add1 data-start) #"")]

    [else
     (values (+ data-start size)
             (subbytes bs data-start (+ data-start size)))]))

(define (decode-uint/16/32 bs start)
  (define-values (data-start size)
    (decode-size bs start))

  (values (+ data-start size) (decode-integer bs data-start size)))

(define (decode-map bs start)
  (define-values (data-start num-pairs)
    (decode-size bs start))

  (let loop ([m (hash)]
             [rem num-pairs]
             [pos data-start])
    (cond
      [(= rem 0)
       (values pos m)]

      [else
       (let*-values ([(vpos k) (decode-field bs pos)]
                     [(npos v) (decode-field bs vpos)])
         (loop (hash-set m k v) (sub1 rem) npos))])))

(define (decode-int32 bs start)
  (define-values (data-start size)
    (decode-size bs start))

  (define unsigned (decode-integer bs (add1 data-start) size))
  (define signed
    (cond
      [(= 1 (arithmetic-shift (bytes-ref bs (add1 data-start)) -7))
       (- unsigned (expt 2 (* 8 size)))]

      [else unsigned]))

  (values (+ data-start size) signed))

(define (decode-uint/64/128 bs start)
  (define-values (data-start size)
    (decode-size bs start))
  (values (add1 (+ data-start size))
          (decode-integer bs (add1 data-start) size)))

(define (decode-array bs start)
  (define-values (data-start num-items)
    (decode-size bs start))

  (let loop ([a null]
             [rem num-items]
             [pos (add1 data-start)]) ;; add1 because it's an extended type
    (cond
      [(= rem 0)
       (values pos (reverse a))]

      [else
       (let-values ([(npos v) (decode-field bs pos)])
         (loop (cons v a) (sub1 rem) npos))])))

(define (decode-dcc bs start)
  (error 'decode-dcc "cannot read data cache containers"))

(define (decode-end-marker bs start)
  (error 'decode-end-marker "cannot read end markers"))

(define (decode-boolean bs start)
  (define-values (_ value)
    (decode-size bs start))
  (values (+ 2 start) (not (= 0 value))))

(define (decode-float bs start)
  (values (+ 7 start)
          (floating-point-bytes->real bs #t
                                      (+ 2 start)
                                      (+ 6 start))))


;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           rackunit/text-ui)

  (define (decode-one-field bs [start 0])
    (let-values ([(_ f) (decode-field bs start)])
      f))

  (define (decode-one-pointer bs [start 0])
    (let-values ([(_ f) (decode-pointer bs start)])
      f))

  (run-tests
   (test-suite
    "decoder"

    (test-suite
     "decode-field"

     (test-suite
      "pointers"

      (check-equal? (decode-one-pointer (bytes #b00100111 #xFF) 0)                2047)
      (check-equal? (decode-one-pointer (bytes #b00101101 #xFF #x10) 0)           (+ 392976 2048))
      (check-equal? (decode-one-pointer (bytes #b00110101 #xFF #x10 #xFF) 0)      (+ 100602111 526336))
      (check-equal? (decode-one-pointer (bytes #b00111101 #xFF #xFF #x10 #xFF) 0) #xFFFF10FF))

     (test-suite
      "strings"

      (check-equal? (decode-one-field (bytes #b01000000)) "")
      (check-equal? (decode-one-field (bytes #b01000010 #xCE #xBB)) "λ"))

     (test-suite
      "doubles"

      (check-equal? (decode-one-field (bytes #b01100000 #x00       #x00       #x00       #x00 #x00 #x00 #x00 #x00)) 0.0)
      (check-equal? (decode-one-field (bytes #b01100000 #b01000000 #b01000101 #b01100000 #x00 #x00 #x00 #x00 #x00)) 42.75))

     (test-suite
      "bytes"

      (check-equal? (decode-one-field (bytes #b10000000)) #"")
      (check-equal? (decode-one-field (bytes #b10000010 #xCE #xBB)) #"\xCE\xBB"))

     (test-suite
      "unsigned integers"

      (check-equal? (decode-one-field (bytes #b10100010 #xFF #x10)) #xFF10)
      ;; TODO: Is this to spec?
      (check-equal? (decode-one-field (bytes #b11000010 #xFF #x00 #xFF #x10)) #xFF00)
      (check-equal? (decode-one-field (bytes #b11000100 #xFF #x00 #xFF #x10)) #xFF00FF10)
      (check-equal? (decode-one-field (bytes #b00001000 2
                                             #xFF #xFF #xAB #xCD #x00 #xFF #x12 #x00))
                    #xFFFFABCD00FF1200))

     (test-suite
      "maps"

      (check-equal? (decode-one-field (bytes #b11100000)) (hash))

      (check-equal? (decode-one-field (bytes #b11100001
                                             #b01000010 65 65
                                             #b10100001 42))
                    (hash "AA" 42))

      (check-equal? (decode-one-field (bytes #b11100010
                                             #b01000010 65 65
                                             #b10100001 42
                                             #b01000010 65 65
                                             #b10100001 24))
                    (hash "AA" 24)))

     (test-suite
      "signed integers"

      (check-equal? (decode-one-field (bytes #b00000100 1
                                             #x00 #xFF #x00 #xFF))
                    #x00FF00FF)

      (check-equal? (decode-one-field (bytes #b00000100 1
                                             #xFF #xFF #xFF #xFF))
                    -1)

      (check-equal? (decode-one-field (bytes #b00000100 1
                                             #x80 #x00 #x00 #x00))
                    -2147483648)

      (check-equal? (decode-one-field (bytes #b00000001 1
                                             #b10010010))
                    -110))

     (test-suite
      "array"

      (check-equal? (decode-one-field (bytes #b00000000 #b00000100)) null)

      (check-equal? (decode-one-field (bytes #b00000001 #b00000100
                                             #b10100001 42))
                    '(42))

      (check-equal? (decode-one-field (bytes #b00000010 #b00000100
                                             #b10100001 42
                                             #b10100001 24))
                    '(42 24)))

     (test-suite
      "booleans"

      (check-eq? (decode-one-field (bytes #b00000000 7)) #f)
      (check-eq? (decode-one-field (bytes #b00000001 7)) #t)
      (check-eq? (decode-one-field (bytes #b00000010 7)) #t))

     (test-suite
      "floats"

      (check-equal? (decode-one-field (bytes #b00000000 8 #x00       #x00       #x00 #x00)) 0.0)
      (check-equal? (decode-one-field (bytes #b00000000 8 #b01000010 #b00101011 #x00 #x00)) 42.75)))

    (test-suite
     "decode-integer"

     (check-equal? (decode-integer #"" 0 0) 0)
     (check-equal? (decode-integer #"\x00" 0 1) 0)
     (check-equal? (decode-integer #"\x10" 0 1) 16)
     (check-equal? (decode-integer #"\xFF\x36" 0 2) #xFF36)
     (check-equal? (decode-integer #"\xFF\xFF\xFF\x10" 0 4) #xFFFFFF10)
     (check-equal? (decode-integer #"\242\377\20" 1 2) #xFF10)))))
