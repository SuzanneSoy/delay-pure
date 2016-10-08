#lang typed/racket

;; This file is used by test-immutable-struct-constructor.rkt .

(provide (struct-out st3-mod)
         (struct-out st4-mod))

(struct st3-mod ([a : Number]) #:transparent)

(struct st4-mod ([a : Number])
  #:constructor-name make-st4-mod
  #:type-name st4-mod-type
  #:transparent)