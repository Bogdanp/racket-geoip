#lang racket/base

(require geoip
         (for-syntax racket/base)
         racket/runtime-path
         rackunit)

(provide integration-tests)

(define-runtime-path country-db-path
  (build-path 'up 'up "fixtures" "dbs" "GeoLite2-Country.mmdb"))

(define-runtime-path city-db-path
  (build-path 'up 'up "fixtures" "dbs" "GeoLite2-City.mmdb"))

(define integration-tests
  (test-suite
   "integration"

   country-tests
   city-tests))

(define (country-data-iso-code result)
  (hash-ref (hash-ref result "country") "iso_code"))

(define (country-data-translation result lang)
  (hash-ref (hash-ref (hash-ref result "country") "names") lang))

(define country-tests
  (test-suite
   "country-db"

   (when (file-exists? country-db-path)
     (define geoip (make-geoip country-db-path))

     (test-case "it returns #f given a private IPv4 address"
       (check-eq? (geoip-lookup geoip "127.0.0.1") #f))

     (test-case "it returns expected results given a known IP address"
       (check-equal? (country-data-iso-code (geoip-lookup geoip "188.24.7.80")) "RO")
       (check-equal? (country-data-translation (geoip-lookup geoip "188.24.7.80") "ru") "Румыния")
       (check-equal? (country-data-translation (geoip-lookup geoip "188.24.7.80") "ja") "ルーマニア")

       (check-equal? (country-data-iso-code  (geoip-lookup geoip "172.217.16.110")) "US")
       (check-equal? (country-data-translation  (geoip-lookup geoip "172.217.16.110") "zh-CN") "美国")))))

(define (city-data-translation result lang)
  (hash-ref (hash-ref (hash-ref result "city") "names") lang))

(define city-tests
  (test-suite
   "city-db"

   (when (file-exists? city-db-path)
     (define geoip (make-geoip city-db-path))

     (test-case "it returns #f given a private IPv4 address"
       (check-eq? (geoip-lookup geoip "127.0.0.1") #f))

     (test-case "it returns expected results given a known IP address"
       (define res (geoip-lookup geoip "188.24.7.80"))
       (check-equal? (city-data-translation res "en") "Cluj-Napoca")
       (check-equal? (city-data-translation res "ja") "クルジュ＝ナポカ")
       (check-equal? (country-data-iso-code (geoip-lookup geoip "188.24.7.80")) "RO")
       (check-equal? (country-data-translation (geoip-lookup geoip "188.24.7.80") "ru") "Румыния")
       (check-equal? (country-data-translation (geoip-lookup geoip "188.24.7.80") "ja") "ルーマニア")))))

(module+ test
  (require rackunit/text-ui)
  (run-tests integration-tests))
