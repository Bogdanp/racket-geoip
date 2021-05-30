#lang racket/base

(require net/ip
         racket/match
         racket/port)

(provide
 make-geoip
 geoip?
 geoip-lookup

 decode-field
 decode-pointer
 decode-integer)


;; Public API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-geoip path)
  (define bs (call-with-input-file path port->bytes))
  (define-values (metadata-marker-start metadata-marker-end)
    (find-metadata-marker bs))

  (define-values (_ metadata)
    (decode-field (subbytes bs metadata-marker-end)))

  (define format-version (hash-ref metadata "binary_format_major_version"))
  (unless (= format-version 2)
    (error 'make-geoip (format "database version ~a not supported" format-version)))

  (define record-size (hash-ref metadata "record_size"))
  (define node-count (hash-ref metadata "node_count"))
  (define tree-size (* (/ (* record-size 2) 8) node-count))
  (define tree (subbytes bs 0 tree-size))
  (define data (subbytes bs (+ 16 tree-size) metadata-marker-start))

  (geoip tree data metadata node-count record-size))

(define (geoip-lookup a-geoip ip)
  (define node-count (geoip-node-count a-geoip))
  (define ip-address (string->geoip-ip-address a-geoip ip))

  (let loop ([bs (ip-address->bytes ip-address)]
             [bits null]
             [index 0])
    (cond
      [(= node-count index) #f]

      [(null? bits)
       (and (not (bytes=? bs #""))
            (loop (subbytes bs 1)
                  (byte->bitlist (bytes-ref bs 0))
                  index))]

      [else
       (define-values (lhs rhs)
         (geoip-tree-ref a-geoip index))

       (define new-index
         (if (zero? (car bits))
             lhs
             rhs))

       (if (> new-index node-count)
           (geoip-data-ref a-geoip new-index)
           (loop bs (cdr bits) new-index))])))


;; Private API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (find-metadata-marker bs)
  (match (regexp-match-positions* #rx#"\xAB\xCD\xEFMaxMind.com" bs)
    [(list _ ... (cons start end)) (values start end)]
    [_ (error 'make-geoip "could not find metadata marker in database file")]))


;;; Tree Lookups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (geoip-tree-ref a-geoip i)
  (define size (geoip-record-size a-geoip))
  (define decoder
    (case size
      [(24) decode-node/24]
      [(28) decode-node/28]
      [(32) decode-node/32]
      [else (error 'geoip-tree-ref "unexpected record size: ~a" size)]))
  (decoder (geoip-tree a-geoip) i))

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
  (define pos (- i (geoip-node-count a-geoip) 16))
  (define-values (_ value)
    (decode-field (geoip-data a-geoip) pos))

  value)

(define (decode-field bs [start 0])
  (define type
    (arithmetic-shift (bytes-ref bs start) -5))
  (case type
    [(1)
     (define-values (pos pointer) (decode-pointer bs start))
     (define-values (_     value) (decode-field bs pointer))
     (values pos value)]

    [(2 3 4 5 6 7)
     (define-values (data-start size)
       (decode-size bs start))

     (case type
       [(2)   (decode-string bs data-start size)]
       [(3)   (decode-double bs data-start size)]
       [(4)   (decode-bytes  bs data-start size)]
       [(5 6) (decode-uint   bs data-start size)]
       [(7)   (decode-map    bs data-start size)])]

    [(0)
     (define subtype
       (+ (bytes-ref bs (add1 start)) 7))
     (define-values (data-start size)
       (decode-size bs start (+ start 2)))
     (case subtype
       [(8)    (decode-int32      bs data-start size)]
       [(9 10) (decode-uint       bs data-start size)]
       [(11)   (decode-array      bs data-start size)]
       [(12)   (decode-dcc        bs data-start size)]
       [(13)   (decode-end-marker bs data-start size)]
       [(14)   (decode-boolean    bs data-start size)]
       [(15)   (decode-float      bs data-start size)]
       [else   (raise-argument-error 'decode-field "(integer-in 8 15)" subtype)])]))

(define (decode-size bs i [npos (add1 i)])
  (define b (bytes-ref bs i))
  (match (bitwise-and #b00011111 b)
    [31 (values (+ 3 npos) (+ (decode-integer bs npos 3) 65821))]
    [30 (values (+ 2 npos) (+ (decode-integer bs npos 2) 285))]
    [29 (values (+ 1 npos) (+ (decode-integer bs npos 1) 29))]
    [s  (values      npos  s)]))

(define (decode-integer bs start size)
  (for/fold ([n 0])
            ([i (in-range size)])
    (+ n (* (bytes-ref bs (+ start i))
            (expt 256 (sub1 (- size i)))))))

(define (decode-pointer bs start)
  (define b (bytes-ref bs start))
  (define ss  (bitwise-and #b11 (arithmetic-shift b -3)))
  (define vvv (bitwise-and #b111 b))

  (case ss
    [(0) (values (+ 2 start)
                 (bitwise-ior (arithmetic-shift vvv 8)
                              (bytes-ref bs (add1 start))))]

    [(1) (values (+ 3 start)
                 (+ 2048
                    (bitwise-ior (arithmetic-shift vvv 16)
                                 (arithmetic-shift (bytes-ref bs (add1 start)) 8)
                                 (bytes-ref bs (+ 2 start)))))]

    [(2) (values (+ 4 start)
                 (+ 526336
                    (bitwise-ior (arithmetic-shift vvv 24)
                                 (arithmetic-shift (bytes-ref bs (+ 1 start)) 16)
                                 (arithmetic-shift (bytes-ref bs (+ 2 start)) 8)
                                 (bytes-ref bs (+ 3 start)))))]

    [(3) (values (+ 5 start)
                 (decode-integer bs (add1 start) 4))]

    [else (raise-argument-error 'decode-pointer "a valid pointer" b)]))

(define (decode-string bs start size)
  (cond
    [(= size 0)
     (values start "")]

    [else
     (define end (+ start size))
     (values end (bytes->string/utf-8 bs #f start end))]))

(define (decode-double bs start size)
  (define end (+ start size))
  (values end (floating-point-bytes->real bs #t start end)))

(define (decode-bytes bs start size)
  (cond
    [(= size 0)
     (values start #"")]

    [else
     (define end (+ start size))
     (values end (subbytes bs start end))]))

(define (decode-uint bs start size)
  (define end (+ start size))
  (values end (decode-integer bs start size)))

(define (decode-map bs start size)
  (let loop ([m (hash)]
             [rem size]
             [pos start])
    (cond
      [(= rem 0)
       (values pos m)]

      [else
       (let*-values ([(vpos k) (decode-field bs pos)]
                     [(npos v) (decode-field bs vpos)])
         (loop (hash-set m k v) (sub1 rem) npos))])))

(define (decode-int32 bs start size)
  (define unsigned
    (decode-integer bs start size))
  (define signed
    (cond
      [(= 1 (arithmetic-shift (bytes-ref bs start) -7))
       (- unsigned (expt 2 (* 8 size)))]

      [else
       unsigned]))

  (values (+ start size) signed))

(define (decode-array bs start size)
  (let loop ([a null]
             [rem size]
             [pos start])
    (cond
      [(= rem 0)
       (values pos (reverse a))]

      [else
       (let-values ([(npos v) (decode-field bs pos)])
         (loop (cons v a) (sub1 rem) npos))])))

(define (decode-dcc _bs _start)
  (error 'decode-dcc "cannot read data cache containers"))

(define (decode-end-marker _bs _start)
  (error 'decode-end-marker "cannot read end markers"))

(define (decode-boolean _bs start size)
  (values start (not (= 0 size))))

(define (decode-float bs start size)
  (define end (+ start size))
  (values end (floating-point-bytes->real bs #t start end)))
