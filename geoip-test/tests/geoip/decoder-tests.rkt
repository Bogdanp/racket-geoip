#lang racket/base

(require geoip/private/decoder
         racket/list
         racket/port
         rackunit)

(provide
 decoder-tests)

(define-check (check-pointer= bs expected-value)
  (let-values ([(_ value) (decode-pointer bs 0)])
    (unless (eqv? value expected-value)
      (with-check-info
        (['value value]
         ['expected expected-value])
        (fail-check)))))

(define-check (check-pos&field bs expected-pos expected-value)
  (let-values ([(pos value) (decode-field bs)])
    (unless (equal? pos expected-pos)
      (with-check-info
        (['pos pos]
         ['expected expected-pos])
        (fail-check)))
    (unless (equal? value expected-value)
      (with-check-info
        (['value value]
         ['expected expected-value])
        (fail-check)))))

(define decoder-tests
  (test-suite
   "decoder"

   (test-suite
    "decode-field"

    (test-suite
     "pointers"

     (check-pointer= (bytes #b00100111 #xFF)                2047)
     (check-pointer= (bytes #b00101101 #xFF #x10)           (+ 392976 2048))
     (check-pointer= (bytes #b00110101 #xFF #x10 #xFF)      (+ 100602111 526336))
     (check-pointer= (bytes #b00111101 #xFF #xFF #x10 #xFF) #xFFFF10FF))

    (test-suite
     "strings"

     (check-pos&field (bytes #b01000000) 1 "")
     (check-pos&field (bytes #b01000101 65 65 65 65 65) 6 "AAAAA")
     (check-pos&field (bytes #b01000010 #xCE #xBB) 3 "λ"))

    (test-suite
     "doubles"

     (check-pos&field (bytes #b01101000 #x00       #x00       #x00       #x00 #x00 #x00 #x00 #x00) 9 0.0)
     (check-pos&field (bytes #b01101000 #b01000000 #b01000101 #b01100000 #x00 #x00 #x00 #x00 #x00) 9 42.75))

    (test-suite
     "bytes"

     (check-pos&field (bytes #b10000000) 1 #"")
     (check-pos&field (bytes #b10000010 #xCE #xBB) 3 #"\xCE\xBB"))

    (test-suite
     "unsigned 16/32-bit integers"

     (check-pos&field (bytes #b10100010 #xFF #x10) 3 #xFF10)
     (check-pos&field (bytes #b11000010 #xFF #x00 #xFF #x10) 3 #xFF00)
     (check-pos&field (bytes #b11000100 #xFF #x00 #xFF #x10) 5 #xFF00FF10))

    (test-suite
     "maps"

     (check-pos&field (bytes #b11100000) 1 (hash))
     (check-pos&field (bytes #b11100001 #b01000010 65 65 #b10100001 42) 6 (hash "AA" 42))
     (check-pos&field (bytes #b11100010 #b01000010 65 65 #b10100001 42 #b01000010 65 65 #b10100001 24) 11 (hash "AA" 24)))

    (test-suite
     "signed integers"

     (check-pos&field (bytes #b00000100 1 #x00 #xFF #x00 #xFF) 6 #x00FF00FF)
     (check-pos&field (bytes #b00000100 1 #xFF #xFF #xFF #xFF) 6 -1)
     (check-pos&field (bytes #b00000100 1 #x80 #x00 #x00 #x00) 6 -2147483648)
     (check-pos&field (bytes #b00000001 1 #b10010010) 3 -110))

    (test-suite
     "unsigned 64/128-bit integers"

     (check-pos&field (bytes #b00001000 #b00000010 #xFF #xEE #xDD #xCC #xBB #xAA #x99 #x88) 10 #xFFEEDDCCBBAA9988)
     (check-pos&field (bytes #b00010000 #b00000011 #xFF #xEE #xDD #xCC #xBB #xAA #x99 #x88 #x77 #x66 #x55 #x44 #x33 #x22 #x11 #x00) 18 #xFFEEDDCCBBAA99887766554433221100))

    (test-suite
     "array"

     (check-pos&field (bytes #b00000000 #b00000100) 2 null)
     (check-pos&field (bytes #b00000001 #b00000100 #b10100001 42) 4 '(42))
     (check-pos&field (bytes #b00000010 #b00000100 #b10100001 42 #b10100001 24) 6 '(42 24))
     (check-pos&field (call-with-output-bytes
                       (lambda (out)
                         (write-bytes (bytes #b00011101 #b00000100 #b00000011) out)
                         (for/list ([_ (in-range 32)])
                           (write-bytes (bytes #b10100001 1) out))))
                      67 (make-list 32 1)))

    (test-suite
     "booleans"

     (check-pos&field (bytes #b00000000 #b00000111) 2 #f)
     (check-pos&field (bytes #b00000001 #b00000111) 2 #t)
     (check-pos&field (bytes #b00000010 #b00000111) 2 #t))

    (test-suite
     "floats"

     (check-pos&field (bytes #b00000100 8 #x00       #x00       #x00 #x00) 6 0.0)
     (check-pos&field (bytes #b00000100 8 #b01000010 #b00101011 #x00 #x00) 6 42.75)))

   (test-suite
    "decode-int32"

    (check-pos&field (bytes #b00000001 #b00000001 #b00000010) 3 2)
    (check-pos&field (bytes #b00000010 #b00000001 #b00000000 #b00000010) 4 2)
    (check-pos&field (bytes #b00000010 #b00000001 #b10000000 #b00000000) 4 -32768))

   (test-suite
    "decode-integer"

    (check-equal? (decode-integer #"" 0 0) 0)
    (check-equal? (decode-integer #"\x00" 0 1) 0)
    (check-equal? (decode-integer #"\x10" 0 1) 16)
    (check-equal? (decode-integer #"\xFF\x36" 0 2) #xFF36)
    (check-equal? (decode-integer #"\xFF\xFF\xFF\x10" 0 4) #xFFFFFF10)
    (check-equal? (decode-integer #"\242\377\20" 1 2) #xFF10))

   #;
   (test-suite
    "property checks"

    (test-case "roundtrip"
      (define gen:value
        (gen:frequency
         `((5 . ,gen:natural)
           (5 . ,gen:real)
           (5 . ,(gen:integer-in (- (expt 2 31)) 0))
           (5 . ,(gen:integer-in 0 (sub1 (expt 2 31))))
           (5 . ,(gen:bytes))
           (5 . ,(gen:string))
           (1 . ,(gen:list (gen:delay gen:value) #:max-length 5))
           (1 . ,(gen:let
                   ([ks (gen:list (gen:string) #:max-length 5)]
                    [vs (gen:list (gen:delay gen:value) #:max-length 5)])
                   (for/hash ([k (in-list ks)]
                              [v (in-list vs)])
                     (values k v)))))))

      (define-property roundtrip
        ([v gen:value])
        (define encoded-v
          (call-with-output-bytes
           (lambda (out)
             (write-field v out))))
        (let-values ([(_ decoded-v) (decode-field encoded-v)])
          (check-equal? decoded-v v)))

      (check-property roundtrip)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests decoder-tests))
