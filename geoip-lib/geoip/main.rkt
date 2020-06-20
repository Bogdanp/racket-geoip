#lang racket/base

(require racket/contract
         "private/decoder.rkt")

(provide
 (contract-out
  [make-geoip (-> path-string? geoip?)]
  [geoip? (-> any/c boolean?)]
  [geoip-lookup (-> geoip? string? (or/c false/c hash?))]))
