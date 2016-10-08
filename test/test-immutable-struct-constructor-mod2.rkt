#lang typed/racket

;; This file is used by test-immutable-struct-constructor.rkt .

(provide (struct-out st5-mod)
         (struct-out st6-mod))

(struct st5-mod ([a : Number]) #:transparent)

(struct st6-mod ([a : Number])
  #:constructor-name make-st6-mod
  #:type-name st6-mod-type
  #:transparent)