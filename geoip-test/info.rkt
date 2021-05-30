#lang info

(define collection 'multi)

(define deps '())
(define build-deps '("base"
                     "geoip-lib"
                     "rackunit-lib"))

(define update-implies '("geoip-lib"))
