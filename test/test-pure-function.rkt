#lang typed/racket

(require delay-pure
         typed/rackunit
         syntax/macro-testing)

(check-equal? (let ([y : Number 123])
                (define f (pure/stateless (λ ([x : Number]) (+ x y))))
                (set! y 0)
                (f 1))
              124)

(check-exn #px"pure: set! is disallowed within pure/stateless and similar forms"
           (λ ()
             (convert-compile-time-error
              (let ()
                (define f (pure/stateless
                           (λ ()
                             (let ([x 1])
                               (λ ()
                                 (let ([y x])
                                   (set! x 0)
                                   y))))))
                (define g (f))
                (list (g) (g))))))

(check-equal? (let ()
                (define f (pure/stateful
                           (λ ()
                             (let ([x 1])
                               (λ ()
                                 (let ([y x])
                                   (set! x 0)
                                   y))))))
                (define g (f))
                (list (g) (g)))
              '(1 0))

;; I'm So Meta Even This Acronym
(check-equal? (let ([x 1])
                (pure/stateless (pure/stateless x)))
              1)
