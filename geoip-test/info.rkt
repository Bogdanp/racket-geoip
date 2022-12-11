#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)
(define deps '())
(define build-deps '("base"
                     "geoip-lib"
                     "rackunit-lib"))
(define update-implies '("geoip-lib"))
