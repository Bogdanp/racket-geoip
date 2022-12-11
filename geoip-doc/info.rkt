#lang info

(define license 'BSD-3-Clause)
(define collection 'multi)
(define deps '("base"))
(define build-deps '("geoip-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("geoip-lib"))
