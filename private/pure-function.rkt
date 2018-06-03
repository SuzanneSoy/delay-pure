#lang typed/racket/base

(require "immutable-struct-constructor.rkt"
         "pure-exception.rkt"
         racket/set
         racket/format
         racket/promise
         (only-in typed/racket/unsafe unsafe-require/typed)
         (prefix-in te: type-expander)
         phc-toolkit
         version-case
         (for-syntax (rename-in racket/base [... …])
                     racket/match
                     syntax/modcollapse
                     racket/list
                     racket/syntax
                     racket/contract
                     syntax/parse
                     syntax/id-set
                     syntax/free-vars
                     type-expander/expander
                     phc-toolkit/untyped
                     "fully-expanded-grammar-no-set.rkt"))

(version-case
 [(version< (version) "6.90.0.29")
  (begin)]
 [else
  (require racket/unsafe/undefined)])

(unsafe-require/typed
 "pure-unsafe.rkt"
 [promise/pure/maybe-stateful? (→ Any Boolean : Promise)]
 [promise/pure/stateless? (→ Any Boolean : Promise)]
 [make-promise/pure/stateful (∀ (a) (→ (→ a) (Promise a)))]
 [make-promise/pure/stateless (∀ (a) (→ (→ a) (Promise a)))]
 [declared-stateful-pure-function? (→ Any Boolean)]
 [declared-stateless-pure-function? (→ Any Boolean)]
 [declared-stateful-pure-function (∀ (A) (→ A A))]
 [declared-stateless-pure-function (∀ (A) (→ A A))])

(unsafe-require/typed
 racket/base
 ;; The type of vector->list was fixed by PR #437, the unsafe-require/typed
 ;; is left for compatibility with earlier versions.
 [vector->list                   (∀ (a) (case→ (→ (Vectorof a) (Listof a))
                                               (→ VectorTop (Listof Any))))]
 [struct-constructor-procedure?  (→ Any Boolean)]
 [struct-predicate-procedure?    (→ Any Boolean)]
 [struct-accessor-procedure?     (→ Any Boolean)])

(unsafe-require/typed racket/struct
                      [[struct->list unsafe-struct->list]
                       (→ Any (Listof Any))])

(provide pure/stateful
         pure/stateless
         pure-thunk/stateful
         pure-thunk/stateless
         define-pure/stateful
         define-pure/stateless
         built-in-pure-functions-set
         (for-syntax built-in-pure-functions-free-id-set)
         promise/pure/maybe-stateful?
         promise/pure/stateless?
         immutable/stateful/c
         immutable/stateless/c
         unsafe-declare-pure/stateless
         unsafe-declare-allowed-in-pure/stateful)

(define-for-syntax built-in-pure-functions-free-id-set
  (immutable-free-id-set
   (syntax->list
    #'(+ - * / modulo add1 sub1 =;; …
         eq? eqv? equal? ;; TODO: equal? can still cause problems if the
         ;; struct's prop:equal+hash is effectful.
         error
         format values
         promise/pure/maybe-stateful? promise/pure/stateless?
         ;; Does not have a type yet:
         ;; list*
         null cons car cdr list list? pair? null? length reverse ;; …
         void
         vector-ref vector-immutable vector-length vector->list vector? ;; …
         hash-ref hash->list hash? ;; …
         set-member? set->list set? ;; …
         ;; allow force, because we only allow capture of free variables
         ;; containing pure stateless promises, which are therefore safe
         ;; to force.
         force
         ;; …
         ))))

(define-for-syntax (built-in-pure-function? id)
  (define (empty-mpi? mpi)
    (equal? (call-with-values (λ () (module-path-index-split mpi))
                              list)
            '(#f #f)))
  (or (free-id-set-member? built-in-pure-functions-free-id-set id)
      (let ([ib (identifier-binding id)])
        (match ib
          ;; circumvent https://github.com/racket/racket/issues/1697
          [(list* (? empty-mpi?) _) #f]
          [(list* (app collapse-module-path-index
                       '(lib "racket/private/kw.rkt"))
                  'make-optional-keyword-procedure
                  _)
           #t]
          [_ #f]))))

(define-syntax (def-built-in-set stx)
  (syntax-case stx ()
    [(_ name)
     #`(define name
         (seteq . #,(free-id-set->list built-in-pure-functions-free-id-set)))]))

(def-built-in-set built-in-pure-functions-set)

(begin
  (define-for-syntax unsafe-pure-functions-free-id-set/stateless
    (mutable-free-id-set))
  (: rw-unsafe-pure-functions-set/stateless (Boxof (Setof Procedure)))
  (define rw-unsafe-pure-functions-set/stateless (box ((inst set Procedure))))
  (define (unsafe-pure-functions-set/stateless)
    (unbox rw-unsafe-pure-functions-set/stateless))
  (define-syntax (unsafe-declare-pure/stateless stx)
    (syntax-case stx ()
      [(_ fn)
       (begin
         (free-id-set-add! unsafe-pure-functions-free-id-set/stateless #'fn)
         #'(set-box! rw-unsafe-pure-functions-set/stateless
                     (set-add (unbox rw-unsafe-pure-functions-set/stateless)
                              fn)))]))
  (define-for-syntax (unsafe-pure-function?/stateless id)
    (free-id-set-member? unsafe-pure-functions-free-id-set/stateless id)))

(begin
  (define-for-syntax unsafe-allowed-functions-free-id-set/stateful
    (mutable-free-id-set))
  (: rw-unsafe-allowed-functions-set/stateful (Boxof (Setof Procedure)))
  (define rw-unsafe-allowed-functions-set/stateful (box ((inst set Procedure))))
  (define (unsafe-allowed-functions-set/stateful)
    (unbox rw-unsafe-allowed-functions-set/stateful))
  (define-syntax (unsafe-declare-allowed-in-pure/stateful stx)
    (syntax-case stx ()
      [(_ fn)
       (identifier? #'fn)
       (begin
         (free-id-set-add! unsafe-allowed-functions-free-id-set/stateful #'fn)
         #'(set-box! rw-unsafe-allowed-functions-set/stateful
                     (set-add (unbox rw-unsafe-allowed-functions-set/stateful)
                              fn)))]))
  (define-for-syntax (unsafe-allowed-function?/stateful id)
    (free-id-set-member? unsafe-allowed-functions-free-id-set/stateful id)))

(: check-immutable/error (→ Variable-Reference
                            (U 'stateful 'stateless)
                            (→ Any Boolean)))
(define ((check-immutable/error varref stateful/stateless) x)
  (check-immutable!
   x
   varref
   stateful/stateless
   (λ () (error (~a "The " x " value was used within a free variable of a pure"
                    " expression or as the result of a pure thunk, but it is"
                    " not immutable.")))
   (λ () (error (~a "The " x " value was used within a free variable of a pure"
                    " expression or as the result of a pure thunk, but I could"
                    " not verify that it is immutable.")))))

(: check-immutable! (→ Any
                       Variable-Reference
                       (U 'stateful 'stateless)
                       (→ Void)
                       (→ Void)
                       Boolean))
(define (check-immutable! x varref stateful/stateless not-immutable other)
  (define (recur y)
    (check-immutable! y varref stateful/stateless not-immutable other))
  (define-syntax-rule (assert x p)
    (if (p x) #t (begin (not-immutable) #f)))
  (cond
    ;; Primitives without recursion
    [(null? x)    #t]
    [(boolean? x) #t]
    [(number? x)  #t]
    [(symbol? x)  #t]
    ;; De facto immutable, with recursion
    [(pair? x) (and (recur (car x))
                    (recur (cdr x)))]
    [(set? x)  (recur (set->list x))]
    ;; Might be immutable, with recursion
    [(string? x) (assert x immutable?)]
    [(bytes? x)  (assert x immutable?)]
    [(box? x)    (and (assert x immutable?)
                      (recur x))]
    [(vector? x) (assert x immutable?)
                 (recur (vector->list x))]
    [(hash? x)   (and (assert x immutable?)
                      (recur (hash->list x)))]
    [(set? x)    (recur (set->list x))]
    ;; Structs:
    [(struct? x) (and (struct-instance-is-immutable? x)
                      (recur (unsafe-struct->list x)))]
    ;; Pure functions
    [((if (eq? stateful/stateless 'stateful)
          declared-stateful-pure-function?
          declared-stateless-pure-function?) x)  #t]
    [(set-member? built-in-pure-functions-set x) #t]
    [(set-member? (unsafe-pure-functions-set/stateless) x) #t]
    [(and (eq? stateful/stateless 'stateful)
          (set-member? (unsafe-allowed-functions-set/stateful) x)) #t]
    ;; delay/pure is only used in a safe way, unless the user requires
    ;; private files
    [(eq? x make-promise/pure/stateful)          #t]
    [(eq? x make-promise/pure/stateless)         #t]
    ;; Pure promises
    ;; We disallow (promise/pure/maybe-stateful? x) because if forced again,
    ;; the outside code may have a handle into some mutable data that we then
    ;; use. promise/pure/stateless? is fine.
    [(promise/pure/stateless? x)                 #t]
    ;; accept struct construtors only if we can guarantee that the struct is
    ;; immutable (this means that the constructor's (object-name) must be
    ;; either 'st or 'make-st, where st is the struct type's name.
    [(immutable-struct-constructor? x varref)    #t]
    [(struct-predicate-procedure? x)             #t]
    [(struct-accessor-procedure? x)              #t]
    ;; To allow pure functions which return pure functions, we need to allow
    ;; check-immutable/c itself
    [(eq? x check-immutable/error)               #t]
    ;; racket/unsafe/undefined is used in the expanded code for functions with
    ;; opetional arguments. We allow it here (for now), even though it is
    ;; unsafe, because the user (or a library) would need to explicitly require
    ;; it to cause problems. Otherwise, it will only appear in code generated by
    ;; typed/racket.
    [(version-case
       [(version< (version) "6.90.0.29") #f]
       [else                             (eq? x unsafe-undefined)])
     #t]
    ;; Otherwise, fail early before mutation causes problems
    [else (begin (other) #f)]))

(: immutable/stateful/c (→ Variable-Reference (→ Any Boolean)))
(define ((immutable/stateful/c varref) x)
  (check-immutable! x varref 'stateful void void))

(: immutable/stateless/c (→ Variable-Reference (→ Any Boolean)))
(define ((immutable/stateless/c varref) x)
  (check-immutable! x varref 'stateless void void))

(define-for-syntax (make-no-set!-transformer id [wrapper #f])
  (λ (stx)
    (syntax-case stx ()
      [(set-id . rest)
       (free-identifier=? #'set-id #'set!)
       (raise-syntax-error
        'pure
        (format (string-append "set! cannot be used in a pure expression to"
                               " mutate the free identifier ~a")
                (syntax-e id))
        stx
        #'set-id)]
      [self (identifier? #'self) (if wrapper #`(#,wrapper #,id) id)]
      [(self . args)
       (identifier? #'self)
       (datum->syntax (syntax-local-identifier-as-binding #'self)
                      `(,(if wrapper #`(#,wrapper #,id) id) . ,#'args))])))

(begin-for-syntax
  (define/contract (pure-impl self fn-stx check-result? stateful/stateless-sym)
    (-> syntax? syntax? (or/c #f 'check-result) (or/c 'stateful 'stateless)
        syntax?)

    (define/with-syntax fn fn-stx)
    (define/with-syntax stateful/stateless stateful/stateless-sym)

    (define/with-syntax fully-expanded+lifts
      ;; TODO: stop on make-predicate (and remove those before free-vars,
      ;; they are safe)
      (local-expand/capture-lifts #'fn 'expression '()))

    (define/with-syntax (fully-expanded (marked-as-unsafe ...))
      (syntax-case #'fully-expanded+lifts (begin)
        [(begin single-expression) #'(single-expression ())]
        [(begin lifted ... expression)
         (for-each (λ (lifted1)
                     (syntax-case lifted1 (define-values
                                            unsafe-pure-block/stateless
                                            unsafe-operation-block/mutating)
                       [(define-values (_)
                          (unsafe-pure-block/stateless . rest))
                        #t]
                       [(define-values (_)
                          (unsafe-operation-block/mutating . rest))
                        (if (not (eq? stateful/stateless-sym 'stateful))
                            (raise-syntax-error
                             'pure
                             (format
                              (string-append "unsafe-operation/mutating"
                                             " disallowed within"
                                             " pure/stateless:\n~a")
                              (syntax->datum lifted1))
                             #'fn
                             lifted1)
                            #t)]
                       [_
                        (raise-syntax-error
                         'pure
                         (format
                          (string-append "lifted expressions are disallowed"
                                         " within pure/stateful, pure/stateless"
                                         " and similar forms (for now):\n~a")
                          (syntax->datum lifted1))
                         #'fn
                         lifted1)]))
                   (syntax->list #'(lifted ...)))
         #'(expression (lifted ...))]))

    (define marked-as-unsafe-ids
      (immutable-free-id-set
       (syntax-case #'(marked-as-unsafe ...) (define-values)
         [((define-values (id ...) _expr) ...)
          (syntax->list #'(id ... ...))])))

    (when (eq? stateful/stateless-sym 'stateless)
      (disallow-set!-in-expression #'fully-expanded))

    (define/with-syntax (free …)
      (filter-not (λ (x)
                    (or (built-in-pure-function? x)
                        (unsafe-pure-function?/stateless x)
                        (and (eq? stateful/stateless-sym 'stateful)
                             (unsafe-allowed-function?/stateful x))
                        (free-id-set-member? marked-as-unsafe-ids x)))
                  (free-vars #'fully-expanded #:module-bound? #t)))

    (define/with-syntax (cached …) (generate-temporaries #'(free …)))

    (define/with-syntax varref (datum->syntax self `(#%variable-reference)))

    #`(let ()
        marked-as-unsafe ...
        (let ([free free] …)
          ;; Prevent the mutation of the cached copy, by making it a macro which
          ;; rejects uses as the target of a set! .
          (let-syntax ([free (make-no-set!-transformer #'free)] …)
            ;; The input should always be stateless
            (assert free (check-immutable/error varref 'stateless))
            …
            ;; The result must be pure too, otherwise it could (I
            ;; suppose) cause problems with occurrence typing, if a
            ;; copy is mutated but not the other, and TR still
            ;; expects them to be equal?
            ;; By construction, it should be immutable, except for functions
            ;; (which can hold internal state), but TR won't assume that when
            ;; called twice, the same function will return the same result. For
            ;; extra security, the result is checked if #:check-result is
            ;; specified. Note that when #:check-result is specified, the pure
            ;; thunk cannot return functions.
            #,(if check-result?
                  #'(λ ()
                      (let ([result (fully-expanded)])
                        ;; The output may be stateful
                        (assert result
                                (check-immutable/error varref
                                                       'stateful/stateless))
                        result))
                  #'fully-expanded))))))

(define-syntax (pure/stateful stx)
  (syntax-case stx ()
    [(self expr)              (pure-impl #'self #'expr #f 'stateful)]))

(define-syntax (pure/stateless stx)
  (syntax-case stx ()
    [(self expr)              (pure-impl #'self #'expr #f 'stateless)]))

(define-syntax (pure-thunk/stateful stx)
  (syntax-case stx ()
    [(self fn)                (pure-impl #'self #'fn #f            'stateful)]
    [(self fn #:check-result) (pure-impl #'self #'fn 'check-result 'stateful)]))

(define-syntax (pure-thunk/stateless stx)
  (syntax-case stx ()
    [(self fn)                (pure-impl #'self #'fn #f            'stateless)]
    [(self fn #:check-result) (pure-impl #'self #'fn 'check-result 'stateless)])
  )

(begin-for-syntax
  (define-syntax-class (maybe-free-id=? other)
    #:attributes ()
    (pattern self:id #:when (or (not other)
                                (free-identifier=? #'self other))))

  (define-syntax-class (name+args+body [other-name #f])
    (pattern ({~and {~optional {~seq #:∀ tvars}} {~seq fa …}}
              ({~var name (maybe-free-id=? other-name)} . args)
              . rest)))
  (define-syntax-class def
    (pattern {~and d {~or {~literal define}
                          {~literal te:define}}}
             #:do [(record-disappeared-uses* #'d)])))

(define-for-syntax ((define-pure/impl stateful/stateless-sym) stx)
  (with-disappeared-uses
   (syntax-parse stx
     [{~or (self {~and whole-τ (CT:colon name/τ:id . self-τ)}
                 (:def . {~var || (name+args+body #'name/τ)}))
           (self . {~and :name+args+body {~not ((:colon . _) . _)}})}
      #:with lam (if (free-identifier=? (datum->syntax #'self 'λ) #'te:λ)
                     (datum->syntax #'here 'te:λ #'name)
                     (datum->syntax #'here 'λ #'name))
      #:with pure/? (if (eq? stateful/stateless-sym 'stateful)
                        #'pure/stateful
                        #'pure/stateless)
      #:with declared-wrapper (if (eq? stateful/stateless-sym 'stateful)
                                  #'declared-stateful-pure-function
                                  #'declared-stateless-pure-function)
      #:with unsafe-free-id-set
      (if (eq? stateful/stateless-sym 'stateful)
          #'unsafe-allowed-functions-free-id-set/stateful
          #'unsafe-pure-functions-free-id-set/stateless)
      #:with name-impl ((make-syntax-introducer) #'name)
      (quasisyntax/top-loc this-syntax
        (begin
          #,@(when-attr CT #'{(CT name-impl . self-τ)})
          ;#,@(when-attr whole-τ #'{whole-τ}) ;; not needed.
          (define-syntax name (make-no-set!-transformer #'name-impl))
          (define name-impl
            (declared-wrapper
             (pure/?
              (lam fa … args . rest))))
          (define-syntax dummy
            ;; Must happen after defining name-impl, so that the fresh
            ;; definition is visible. Due to the way Racket handle intdef-ctx
            ;; it will first run all the macro definitions, and then expand the
            ;; contents of name-impl (so when expanding the pure/? code,
            ;; the free-id-set will already be modified.
            (free-id-set-add! unsafe-free-id-set #'name-impl))))])))

(define-syntax define-pure/stateful (define-pure/impl 'stateful))
(define-syntax define-pure/stateless (define-pure/impl 'stateless))
