#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("geoip-lib"
                     "racket-doc"
                     "scribble-lib"))
(define update-implies '("geoip-lib"))
