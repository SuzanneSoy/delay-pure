#lang typed/racket

(provide promise/pure/maybe-stateful?
         promise/pure/stateless?
         delay/pure/stateful
         delay/pure/stateless)

(require typed/racket/unsafe
         "pure-function.rkt"
         racket/private/promise
         (for-syntax version-case
                     racket/base
                     syntax/parse
                     phc-toolkit/untyped)
         (for-meta 2 racket/base))

(unsafe-require/typed
 "pure-unsafe.rkt"
 [make-promise/pure/stateful (∀ (a) (→ (→ a) (Promise a)))]
 [make-promise/pure/stateless (∀ (a) (→ (→ a) (Promise a)))])

(define-for-syntax (stx-e x)
  (if (syntax? x) (syntax-e x) x))

(define-syntax (delay/pure/stateless/unsafe stx)
  (version-case
   [(version< (version) "7.4")
    (make-delayer stx #'make-promise/pure/stateless '())]
   [else
    (delayer (#'make-promise/pure/stateless '()) stx)]))
  
(define-syntax delay/pure/stateful
  (syntax-parser
    [(_ e)
     (syntax/top-loc this-syntax
       (make-promise/pure/stateful
        (pure-thunk/stateful (λ () e))))]))

(define-syntax delay/pure/stateless
  (syntax-parser
    [(_ e)
     (syntax/top-loc this-syntax
       (make-promise/pure/stateless
        (pure-thunk/stateless (λ () e))))]))
