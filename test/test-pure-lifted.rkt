#lang typed/racket

(require delay-pure
         typed/rackunit
         syntax/macro-testing)

(define-syntax (lft stx)
  (syntax-case stx ()
    [(_ e)
     (syntax-local-lift-expression #'e)]))

(check-exn (regexp (string-append "lifted expressions are disallowed within"
                                  " pure/stateful, pure/stateless and similar"
                                  " forms \\(for now\\)"))
           (λ ()
             (convert-compile-time-error
              (let ()
                (define x 1)
                (pure/stateless (+ 2 (lft (begin (set! x (add1 x)) x)) 3))))))



(check-exn (regexp (string-append "lifted expressions are disallowed within"
                                  " pure/stateful, pure/stateless and similar"
                                  " forms \\(for now\\)"))
           (λ ()
             (convert-compile-time-error
              (let ()
                (define x 1)
                (pure/stateful (+ 2 (lft (begin (set! x (add1 x)) x)) 3))))))