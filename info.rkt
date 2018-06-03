#lang info
(define collection "delay-pure")
(define deps '("base"
               "rackunit-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "type-expander"
               "phc-toolkit"
               "version-case"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "typed-racket-doc"))
(define scribblings '(("scribblings/delay-pure.scrbl" () ("typed-racket"))))
(define pkg-desc
  (string-append "Non-cached promises for Typed/Racket, like delay/name."
                 " Should be sound for occurrence typing (unlike"
                 " delay/name) because only pure functions are allowed."))
(define version "1.0")
(define pkg-authors '("Georges Dupéron"))
