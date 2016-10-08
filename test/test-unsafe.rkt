#lang typed/racket

(require delay-pure
         syntax/macro-testing
         typed/rackunit)

;; unsafe-pure/stateless within pure-thunk/stateless
(let ()
  (define x 1)
  (define f
    (pure-thunk/stateless
     (λ ()
       (+ (unsafe-pure/stateless (set! x (add1 x))
                                 x)
          1))))
  (check-equal? (list (f) (f) (f)) '(3 4 5)))

;; unsafe-pure/stateless within pure-thunk/stateful
(let ()
  (define x 1)
  (define f
    (pure-thunk/stateful
     (λ ()
       (+ (unsafe-pure/stateless (set! x (add1 x))
                                 x)
          1))))
  (check-equal? (list (f) (f) (f)) '(3 4 5)))

;; unsafe-operation/mutating within pure-thunk/stateless
(check-exn #px"unsafe-operation/mutating disallowed within pure/stateless"
           (λ ()
             (convert-compile-time-error
              (let ()
                (define x 1)
                (define f
                  (pure-thunk/stateless
                   (λ ()
                     (+ (unsafe-operation/mutating (set! x (add1 x))
                                                   x)
                        1))))
                (check-equal? (list (f) (f) (f)) '(3 4 5))))))

;; unsafe-operation/mutating within pure-thunk/stateful
(let ()
  (define x 1)
  (define f
    (pure-thunk/stateful
     (λ ()
       (+ (unsafe-operation/mutating (set! x (add1 x))
                                     x)
          1))))
  (check-equal? (list (f) (f) (f)) '(3 4 5)))
