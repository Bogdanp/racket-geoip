#lang racket/base

(require (prefix-in b: racket/base))

(provide
 write-field)

(define (write-field v [out (current-output-port)])
  (cond
    [(bytes?   v) (write-bytes   v out)]
    [(string?  v) (write-string  v out)]
    [(hash?    v) (write-map     v out)]
    [(list?    v) (write-array   v out)]
    [(boolean? v) (write-boolean v out)]

    [(exact-integer? v)
     (cond
       [(<  v 0) (write-int32 v out)]
       [(<= v (sub1 (expt 2 16)))  (write-uint16 v out)]
       [(<= v (sub1 (expt 2 32)))  (write-uint32 v out)]
       [(<= v (sub1 (expt 2 64)))  (write-uint64 v out)]
       [(<= v (sub1 (expt 2 128))) (write-uint128 v out)]
       [else (raise-argument-error 'write-field "(integer-in (- (sub1 expt 2 31)) (sub1 (expt 2 128)))" v)])]

    [(real? v)
     (write-double  v out)]

    [(and (pair? v) (eq? 'pointer (car v)))
     (write-pointer (cdr v) out)]

    [else
     (raise-argument-error 'write-field "a valid field" v)]))

(define write-control
  (case-lambda
    [(type size)
     (write-control type #f size (current-output-port))]

    [(type size out)
     (write-control type #f size out)]

    [(type subtype size out)
     (cond
       [(>= size 65821)
        (let ([size (- size 65821)])
          (write-byte (bitwise-ior type #b00011111) out)
          (when subtype (write-byte (- subtype 7) out))
          (write-integer size 3 out))]

       [(>= size 285)
        (let ([size (- size 285)])
          (write-byte (bitwise-ior type #b00011110) out)
          (when subtype (write-byte (- subtype 7) out))
          (write-integer size 2 out))]

       [(>= size 29)
        (let ([size (- size 29)])
          (write-byte (bitwise-ior type #b00011101) out)
          (when subtype (write-byte (- subtype 7) out))
          (write-integer size 1 out))]

       [else
        (b:write-byte (bitwise-ior type size) out)
        (when subtype (write-byte (- subtype 7) out))])]))

(define (write-integer n size [out (current-output-port)])
  (let loop ([bs null] [n n] [remaining size])
    (cond
      [(zero? remaining)
       (for ([b (in-list bs)])
         (b:write-byte b out))]

      [else
       (loop (cons (remainder n 256) bs)
             (quotient n 256)
             (sub1 remaining))])))

(define (write-pointer p [out (current-output-port)])
  (cond
    [(>= p 134744063) ;; 2^27-1 + 526336
     (b:write-byte #b00111100 out)
     (write-integer p 4 out)]

    [(>= p 526336)
     (let ([p (- p 526336)])
       (b:write-byte (bitwise-ior #b00110000 (arithmetic-shift p -24)) out)
       (b:write-byte (bitwise-and #xFF       (arithmetic-shift p -16)) out)
       (b:write-byte (bitwise-and #xFF       (arithmetic-shift p  -8)) out)
       (b:write-byte (bitwise-and #xFF                         p     ) out))]

    [(>= p 2048)
     (let ([p (- p 2048)])
       (b:write-byte (bitwise-ior #b00101000 (arithmetic-shift p -16)) out)
       (b:write-byte (bitwise-and #xFF       (arithmetic-shift p  -8)) out)
       (b:write-byte (bitwise-and #xFF                         p     ) out))]

    [else
     (b:write-byte (bitwise-ior #b00100000 (arithmetic-shift p -8)) out)
     (b:write-byte (bitwise-and #xFF                         p    ) out)]))

(define (write-string s [out (current-output-port)])
  (define bs (string->bytes/utf-8 s))
  (define len (bytes-length bs))
  (write-control #b01000000 len out)
  (b:write-bytes bs out))

(define (write-double n [out (current-output-port)])
  (write-control #b01100000 8 out)
  (b:write-bytes (real->floating-point-bytes n 8 #t) out))

(define (write-bytes s [out (current-output-port)])
  (define len (bytes-length s))
  (write-control #b10000000 len out)
  (b:write-bytes s out))

(define (write-uint16 n [out (current-output-port)])
  (write-control #b10100000 2 out)
  (write-integer n 2 out))

(define (write-uint32 n [out (current-output-port)])
  (write-control #b11000000 4 out)
  (write-integer n 4 out))

(define (write-map h [out (current-output-port)])
  (define len (hash-count h))
  (write-control #b11100000 len out)
  (for ([(k v) (in-hash h)])
    (write-string k out)
    (write-field v out)))

(define (write-int32 n [out (current-output-port)])
  (define reinterpreted
    (integer-bytes->integer
     (integer->integer-bytes n 4 #t #t)
     #f #t))
  (write-control 0 8 4 out)
  (write-integer reinterpreted 4 out))

(define (write-uint64 n [out (current-output-port)])
  (write-control 0 9 8 out)
  (write-integer n 2 out))

(define (write-uint128 n [out (current-output-port)])
  (write-control 0 10 16 out)
  (write-integer n 4 out))

(define (write-array vs [out (current-output-port)])
  (define len (length vs))
  (write-control 0 11 len out)
  (for ([v (in-list vs)])
    (write-field v out)))

(define (write-boolean v [out (current-output-port)])
  (write-control 0 14 (if v 1 0) out))
