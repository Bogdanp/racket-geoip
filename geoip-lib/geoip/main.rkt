#lang racket/base

(require racket/contract/base
         "private/decoder.rkt")

(provide
 (contract-out
  [make-geoip (-> path-string? geoip?)]
  [geoip? (-> any/c boolean?)]
  [geoip-metadata (-> geoip? (hash/c string? any/c))]
  [geoip-lookup (-> geoip? string? (or/c #f hash?))]))
