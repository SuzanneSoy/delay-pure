#lang typed/racket

(provide unsafe-pure-block/stateless
         unsafe-operation-block/mutating
         unsafe-pure/stateless
         unsafe-operation/mutating)

(module m typed/racket
  (provide unsafe-pure-block/stateless
           unsafe-operation-block/mutating)
  (define-syntax-rule (unsafe-pure-block/stateless . body)
    (λ () . body))
  (define-syntax-rule (unsafe-operation-block/mutating . body)
    (λ () . body)))

(require 'm)

(define-syntax (unsafe-pure/stateless stx)
  (syntax-case stx ()
    [(_ . body)
     (with-syntax ([lifted-id (syntax-local-lift-expression
                               #'(unsafe-pure-block/stateless . body))])
       #'(lifted-id))]))

(define-syntax (unsafe-operation/mutating stx)
  (syntax-case stx ()
    [(_ . body)
     (with-syntax ([lifted-id (syntax-local-lift-expression
                               #'(unsafe-operation-block/mutating . body))])
       #'(lifted-id))]))