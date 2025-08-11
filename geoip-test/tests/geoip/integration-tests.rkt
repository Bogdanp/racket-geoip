#lang racket/base

(require (for-syntax racket/base)
         geoip
         racket/runtime-path
         rackunit)

(provide
 integration-tests)

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
     (define g (make-geoip country-db-path))

     (test-case "it returns #f given a private IPv4 address"
       (check-eq? (geoip-lookup g "127.0.0.1") #f))

     (test-case "it returns expected results given a known IP address"
       (check-equal? (country-data-iso-code (geoip-lookup g "188.24.7.80")) "RO")
       (check-equal? (country-data-translation (geoip-lookup g "188.24.7.80") "ru") "Румыния")
       (check-equal? (country-data-translation (geoip-lookup g "188.24.7.80") "ja") "ルーマニア")

       (check-equal? (country-data-iso-code  (geoip-lookup g "172.217.16.110")) "US")
       (check-equal? (country-data-translation  (geoip-lookup g "172.217.16.110") "zh-CN") "美国")))))

(define (city-data-translation result lang)
  (let* ([city (hash-ref result "city" #f)]
         [names (and city (hash-ref city "names" #f))])
    (and names (hash-ref names lang))))

(define city-tests
  (test-suite
   "city-db"

   (when (file-exists? city-db-path)
     (define g (make-geoip city-db-path))

     (test-case "it returns the metadata"
       (check-equal? (hash-ref (geoip-metadata g) "database_type") "GeoLite2-City"))

     (test-case "it returns #f given a private IPv4 address"
       (check-eq? (geoip-lookup g "127.0.0.1") #f))

     (test-case "it returns expected results given a known IP address"
       (define res (geoip-lookup g "188.24.7.80"))
       (check-equal? (city-data-translation res "en") "Cluj-Napoca")
       (check-equal? (city-data-translation res "ja") "クルジュ＝ナポカ")
       (check-equal? (country-data-iso-code (geoip-lookup g "188.24.7.80")) "RO")
       (check-equal? (country-data-translation (geoip-lookup g "188.24.7.80") "ru") "Румыния")
       (check-equal? (country-data-translation (geoip-lookup g "188.24.7.80") "ja") "ルーマニア"))

     (test-case "it finds known addresses"
       (define addrs
         '("127.0.0.1"
           "188.24.7.80"
           "67.212.234.157"
           "172.58.175.36"
           "179.43.102.6"
           "185.244.96.150"
           "95.148.7.63"
           "81.174.148.38"
           "156.146.56.131"
           "91.175.157.15"
           "88.127.127.97"))

       (define expected
         '(#f
           "Cluj-Napoca"
           "Salt Lake City"
           "Orlando"
           "Belén de Escobar"
           "Gdansk"
           "Milton Keynes"
           "Sheffield"
           "Singapore"
           "Jacou"
           "Antony"))

       (check-equal?
        (for/list ([addr (in-list addrs)])
          (define data (geoip-lookup g addr))
          (and data (city-data-translation data "en")))
        expected)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests integration-tests))
