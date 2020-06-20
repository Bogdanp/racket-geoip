#lang racket/base

(require rackunit
         "decoder-tests.rkt"
         "integration-tests.rkt")

(define all-tests
  (test-suite
   "geoip"

   decoder-tests
   integration-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-tests))
