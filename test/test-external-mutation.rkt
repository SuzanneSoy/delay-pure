#lang typed/racket
(require delay-pure
         typed/rackunit)

;; This file checks that externally mutating a free variable on which a pure
;; function or promise depends does not affect the function's result.
(check-equal? (let ([x 1])
                (define d (delay/pure/stateful (add1 x)))
                (list (begin (set! x -10) (force d))
                      (begin (set! x -11) (force d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define d (delay/pure/stateless (add1 x)))
                (list (begin (set! x -10) (force d))
                      (begin (set! x -11) (force d))))
              '(2 2))

;; pure/stateless and pure/stateful do not protect the expression from
;; external mutations, so we are not testing this case here.

(check-equal? (let ([x 1])
                (define d (pure-thunk/stateless (λ () (add1 x))))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define d (pure-thunk/stateless (λ () (add1 x)) #:check-result))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define d (pure-thunk/stateful (λ () (add1 x))))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define d (pure-thunk/stateful (λ () (add1 x)) #:check-result))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define-pure/stateless (d) (add1 x))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define-pure/stateless (d [opt : Number x]) (add1 opt))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define-pure/stateless (d #:kw [opt : Number x]) (add1 opt))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define-pure/stateful (d) (add1 x))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define-pure/stateful (d [opt : Number x]) (add1 opt))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

(check-equal? (let ([x 1])
                (define-pure/stateful (d #:kw [opt : Number x]) (add1 opt))
                (list (begin (set! x -10) (d))
                      (begin (set! x -11) (d))))
              '(2 2))

;; Check that this doesn't cause a run-time error due to the internal use of
;; unsafe-undefined in the expanded function with optional arguments (starting
;; from Racket 6.90.0.29)
(define z 1)
(define-pure/stateless (d [opt : Number z]) (void))
(d)