#lang racket/base

(require rackunit
         "integration-tests.rkt")

(define all-tests
  (test-suite
   "geoip"

   integration-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests all-tests))
