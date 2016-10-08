#lang typed/racket

(provide promise/pure/maybe-stateful?
         promise/pure/stateless?
         delay/pure/stateful
         delay/pure/stateless)

(require typed/racket/unsafe
         "pure-function.rkt"
         racket/private/promise
         (for-syntax racket/base
                     syntax/parse
                     phc-toolkit/untyped))

(unsafe-require/typed
 "pure-unsafe.rkt"
 [make-promise/pure/stateful (∀ (a) (→ (→ a) (Promise a)))]
 [make-promise/pure/stateless (∀ (a) (→ (→ a) (Promise a)))])

(define-syntax (delay/pure/stateful/unsafe stx)
  (make-delayer stx #'make-promise/pure/stateful '()))

(define-syntax (delay/pure/stateless/unsafe stx)
  (make-delayer stx #'make-promise/pure/stateless '()))
  
(define-syntax delay/pure/stateful
  (syntax-parser
    [(_ e)
     (syntax/top-loc this-syntax
       (delay/pure/stateful/unsafe (pure/stateful e)))]))

(define-syntax delay/pure/stateless
  (syntax-parser
    [(_ e)
     (syntax/top-loc this-syntax
       (delay/pure/stateless/unsafe (pure/stateless e)))]))
