#lang typed/racket

(require delay-pure/private/immutable-struct-constructor
         "test-immutable-struct-constructor-mod.rkt"
         typed/rackunit)

(struct st1 ([a : Number]) #:transparent)

(struct st2 ([a : Number])
  #:constructor-name make-st2
  #:type-name st2-type
  #:transparent)

;; From this module
(check-true (immutable-struct-constructor? st1 (#%variable-reference)))
(check-true (immutable-struct-constructor? make-st2 (#%variable-reference)))
;; From another module
(check-true (immutable-struct-constructor? st3-mod (#%variable-reference)))
(check-true (immutable-struct-constructor? make-st4-mod (#%variable-reference)))

;; From a macro
(define-syntax (test-from-macro _)
  #'(begin
      ;; From this module
      (check-true (immutable-struct-constructor? st1
                                                 (#%variable-reference)))
      (check-true (immutable-struct-constructor? make-st2
                                                 (#%variable-reference)))
      ;; From another module
      (check-true (immutable-struct-constructor? st3-mod
                                                 (#%variable-reference)))
      (check-true (immutable-struct-constructor? make-st4-mod
                                                 (#%variable-reference)))))

(test-from-macro)

;; From a macro, using a module which is required by the macro
(define-syntax (test-required-from-macro _)
  #'(begin
      (require "test-immutable-struct-constructor-mod2.rkt")
      ;; From another module
      (check-true (immutable-struct-constructor? st3-mod
                                                 (#%variable-reference)))
      (check-true (immutable-struct-constructor? make-st4-mod
                                                 (#%variable-reference)))))

(test-required-from-macro)
