#lang typed/racket

(module test typed/racket
  (require delay-pure
           typed/rackunit)

  (check-equal? (force (let ([x (vector-immutable 1 2 3)])
                         (delay/pure/stateless (vector-ref x 0))))
                1)

  (check-equal? (force (force (let ([x (vector-immutable 1 2 3)])
                                (delay/pure/stateless
                                 (delay/pure/stateless (vector-ref x 0))))))
                1)

  (define f0
    (let ([x (vector-immutable 'a 'b 'c)])
      (let ()
        (define-pure/stateless
          (: f (→ Integer
                  (Listof Integer)
                  (Rec R (List* Integer Symbol (Promise R)))))
          (define (f [n : Integer] [big : (Listof Integer)])
            : (Rec R (List* Integer Symbol (Promise R)))
            (cons (length big)
                  (cons (vector-ref x (modulo n 3))
                        (delay/pure/stateless (f (add1 n)
                                                 (reverse (cons (length big)
                                                                big))))))))
        (f 0 '()))))

  ;; Check that the first 100 elements are as expected:
  (check-equal? (let rec : (Listof (List Integer Symbol)) ([i 0] [fi f0])
                  (if (>= i 100)
                      '()
                      (cons (list (car fi) (cadr fi))
                            (rec (add1 i) (force (cddr fi))))))
                (for/list : (Listof (List Integer Symbol))
                  ([a (in-list (range 100))]
                   [b (in-cycle (in-list '(a b c)))])
                  (list a b)))

  (: travel-far (→ Number
                   (Rec R (List* Integer Symbol (Promise R)))
                   (List Integer Symbol)))
  (define (travel-far i fi)
    (if (= i 0)
        (list (car fi) (cadr fi))
        (travel-far (sub1 i) (force (cddr fi)))))

  (check-equal? (travel-far 0 f0) '(0 a))
  (check-equal? (travel-far 1 f0) '(1 b))
  (check-equal? (travel-far 2 f0) '(2 c))
  (check-equal? (travel-far 3 f0) '(3 a))
  (check-equal? (travel-far 99 f0) '(99 a))
  (check-equal? (travel-far 1000 f0) '(1000 b))

  ;; Test showing that the promise is not cached: we follow a very long sequence
  ;; of recursive promises, while still holding a reference to the first in f0,
  ;; if caching occurs we will go out of memory.
  ;; Since the state of each promise contains a fresh list (reversed from the
  ;; previous promise's state with an extra element consed in front), the total
  ;; size would be quadratic in the number of steps if all the intermediate
  ;; promises were cached.
  ;; This test runs for about 75 seconds on my machine. It allocates at least
  ;; (* 50000 50000 1/2) cons cells, which, with 64 bits = 8 bytes per cons cell
  ;; amounts to (/ (* 50000 50000 1/2 8) (* 1024 1024 1024)) ≅ 9 GiB of RAM,
  ;; which should be enough to make the travis build fail if caching occurs.
  (module config info
    (define timeout 600))
  (check-equal? (travel-far 50000 f0) '(50000 c)))