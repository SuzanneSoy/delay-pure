#lang typed/racket

(require typed/racket/unsafe
         (for-syntax racket/struct-info
                     racket/list
                     racket/function)
         (for-template phc-toolkit/untyped/meta-struct)
         phc-toolkit)
(unsafe-require/typed racket/base
                      [struct-constructor-procedure?  (→ Any Boolean)])
  
(provide immutable-struct-constructor?)
  
(: immutable-struct-constructor? (→ Any Variable-Reference Boolean))
(define (immutable-struct-constructor? v vr)
  (and (struct-constructor-procedure? v)
       (let ([s-name (object-name v)])
         (and (symbol? s-name)
              (or (immutable-struct?/symbol s-name v vr)
                  (let ([mk-s (regexp-match #px"^make-(.*)$"
                                            (symbol->string s-name))])
                    (and mk-s (pair? (cdr mk-s)) (cadr mk-s)
                         (let ([sym (string->symbol (cadr mk-s))])
                           (immutable-struct?/symbol sym v vr)))))))))

(define-syntax (meta-struct-immutable stx)
  (syntax-case stx ()
    [(_ ident)
     (let ()
       (define slv (syntax-local-value #'ident (λ () #f)))
       (if (and slv
                (struct-info? slv)
                (let ([esi (extract-struct-info slv)])
                  (and (last (fourth esi))
                       (not (ormap identity (fifth esi))))))
           #'#t
           #'#f))]))

(define-syntax (meta-struct-type-descriptor stx)
  (syntax-case stx ()
    [(_ ident)
     (let ()
       (define slv (syntax-local-value #'ident (λ () #f)))
       #`#,(and slv
                (struct-info? slv)
                (first (extract-struct-info slv))))]))

(define-syntax (meta-struct-constructor stx)
  (syntax-case stx ()
    [(_ ident)
     (let ()
       (define slv (syntax-local-value #'ident (λ () #f)))
       #`#,(and slv
                (struct-info? slv)
                (second (extract-struct-info slv))))]))

(define (raco-test-exn? [e : exn:fail:contract])
  ;; See TR issue #439 at https://github.com/racket/typed-racket/issues/439
  (regexp-match #px"Attempted to use a struct type reflectively in untyped code"
                (exn-message e)))

(: immutable-struct?/symbol (→ Symbol Any Variable-Reference Boolean))
(define (immutable-struct?/symbol sym ctor vr)
  (define meta-result
    (call-with-values
     (λ ()
       (eval `(,#'list* (,#'meta-struct-immutable ,sym)
                        (,#'meta-struct-type-descriptor ,sym)
                        (,#'meta-struct-constructor ,sym))
             (variable-reference->namespace vr)))
     (λ l l)))
  (and (pair? meta-result)
       (pair? (car meta-result))
       (pair? (cdar meta-result))
       (let ([meta-probably-immutable? (equal? (caar meta-result) #t)]
             [meta-descriptor (cadar meta-result)]
             [meta-constructor (cddar meta-result)])
         (and meta-probably-immutable?
              meta-descriptor
              (struct-type? meta-descriptor)
              ;; double-check, meta-probably-immutable? could be true if we
              ;; use a constructor named make-st, but st is actually bound to a
              ;; different struct.
              (let ([try-immutable-struct-type
                     : (U #t #f 'raco-test-exn)
                     (with-handlers ([exn:fail:contract?
                                      (λ ([e : exn:fail:contract])
                                        (if (raco-test-exn? e)
                                            'raco-test-exn
                                            #f))])
                       (if (struct-type-is-immutable? meta-descriptor)
                           #t
                           #f))])
                (cond
                  [(eq? try-immutable-struct-type #t)
                   ;; double-check that the heuristic worked, and that the
                   ;; guessed struct's constructor is indeed the original one:
                   (eq? meta-constructor ctor)]
                  [(eq? try-immutable-struct-type 'raco-test-exn)
                   ;; the (eq? meta-constructor ctor) does not work properly
                   ;; with raco test either
                   #t]
                  [(eq? try-immutable-struct-type #f)
                   #f]))))))
