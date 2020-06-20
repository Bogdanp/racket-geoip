#lang racket/base

(require (for-syntax racket/base)
         (submod geoip/private/decoder testing)
         racket/runtime-path
         rackunit)

(provide
 decoder-tests)

(define (decode-one-field bs [start 0])
  (define-values (_ value) (decode-field bs start))
  value)

(define (decode-one-pointer bs [start 0])
  (define-values (_ value) (decode-pointer bs start))
  value)

(define decoder-tests
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
    (check-equal? (decode-integer #"\242\377\20" 1 2) #xFF10))))

(module+ test
  (require rackunit/text-ui)
  (run-tests decoder-tests))
