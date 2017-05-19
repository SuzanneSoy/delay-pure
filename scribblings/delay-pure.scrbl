#lang scribble/manual
@require[racket/require
         delay-pure
         @for-syntax[racket/base
                     syntax/id-set]
         @for-label[delay-pure
                    racket
                    (except-in (subtract-in typed/racket racket) :)
                    racket/promise
                    (only-in type-expander :)]]

@title{Pure functions and promises}
@author[@author+email["Georges Dupéron" "georges.duperon@gmail.com"]]

@defmodule[delay-pure]

@deftogether[[@defform[(delay/pure/stateless expression)]
              @defform[(delay/pure/stateful expression)]]]{

 Produces a promise for @racket[expression] which does not cache its result,
 like @racket[delay/name]. The @racket[delay/pure/stateless] form checks that
 the @racket[expression] is pure by wrapping it with
 @racket[(pure/stateless expression)]. The @racket[delay/pure/stateful] form
 instead relies on @racket[(pure/stateful expression)].}

@defproc[(promise/pure/maybe-stateful? [v any/c]) boolean?]{
                                                        
 A predicate which recognizes promises created with both
 @racket[delay/pure/stateless] and @racket[delay/pure/stateful].}

@defproc[(promise/pure/stateless? [v any/c]) boolean?]{
 A predicate which recognizes promises created with
 @racket[delay/pure/stateless], and rejects those created with
 @racket[delay/pure/stateful].}
         

@deftogether[[@defform[(pure/stateless expression)]
              @defform[(pure/stateful expression)]]]{
                                         
 Checks that the @racket[expression] is pure. This is done by fully expanding
 the expression, and checking at run-time that the free variables (including
 functions) contain only immutable values and pure functions. There is a
 hard-coded list of built-in functions which are known to be pure. The
 functions created with @racket[define-pure/stateless] are also accepted (but
 not those created with @racket[define-pure/stateful]), as well as
 @racket[struct] accessors and predicates, and @racket[struct] constructors for
 immutable structures.

 Note that the expressions can refer to variables mutated with @racket[set!]
 by other code. Placing the expression in a lambda function and calling that
 function twice may therefore yield different results, if other code mutates
 some free variables between the two invocations. In order to produce a pure
 thunk which caches its inputs (thereby shielding them from any mutation of the
 external environment), use @racket[pure-thunk/stateless] and
 @racket[pure-thunk/stateful] instead.

 The first form, @racket[pure/stateless], checks that once fully-expanded, the
 @racket[expression] does not contain uses of @racket[set!]. Since the free
 variables can never refer to stateful functions, this means that any function
 present in the result is guaranteed be a @deftech{stateless} function. The
 results of two calls to a @tech{stateless} function with the same arguments
 should be indistinguishable, aside from the fact that they might not be
 @racket[eq?]. In other words, a @tech{stateless} function will always return
 the ``same'' (not necessarily @racket[eq?]) value given the same
 (@racket[eq?]) arguments. If the result contains functions, these functions are
 guaranteed to be @tech{stateless} too.

 With the second form @racket[pure/stateful], uses of @racket[set!] are
 allowed within the expression (but may not alter free variables). The
 resulting value will be an immutable value which may contain both
 @tech{stateless} and @deftech{stateful} functions. Stateful functions may be
 closures over a value which is mutated using @racket[set!], and therefore
 calling a @tech{stateful} function twice with the same (@racket[eq?])
 arguments may produce different results. Since Typed/Racket does not use
 occurrence typing on function calls, the guarantee that the result is
 immutable until a function value is reached is enough to safely build
 non-caching promises that return the ``same'' value, as far as occurrence
 typing is concerned.

 Promises created with @racket[delay/pure/stateless] and
 @racket[delay/pure/stateful] re-compute their result each time, which yields
 results that are not necessarily @racket[eq?]. This means that calling
 @racket[eq?] twice on the same pair of expressions may not produce the same
 result. Fortunately, occurrence typing in Typed/Racket does not rely on this
 assumption, and does not "cache" the result of calls to @racket[eq?]. If this
 behaviour were to change, this library would become unsound.

 TODO: add a test in the test suite which checks that Typed/Racket does not
 "cache" the result of @racket[eq?] calls, neither at the type level, nor at
 the value level.}

@deftogether[[@defform*[[(pure-thunk/stateless thunk)
                         (pure-thunk/stateless thunk #:check-result)]]
              @defform*[[(pure-thunk/stateful thunk)
                         (pure-thunk/stateful thunk #:check-result)]]]]{
           
 Like @racket[pure/stateless] and @racket[pure/stateful], but the
 @racket[_thunk] expression should produce a thunk. When
 @racket[#:check-result] is specified, a run-time guard on the function's
 result is added. The guard checks that the result is an immutable value. With
 @racket[pure-thunk/stateless], the result guard only accepts immutable values,
 possibly containing @tech{stateless} functions. With
 @racket[pure-thunk/stateful], the result guard also accepts immutable values,
 possibly containing @tech{stateful} functions.}

@deftogether[
 [@defform*[#:literals (:)
            [(define-pure/stateless (name . args) maybe-result body ...)
             (define-pure/stateless
               (: name . type)
               (define (name . args) maybe-result body ...))]]
  @defform*[#:literals (:)
            [(define-pure/stateful (name . args) maybe-result body ...)
             (define-pure/stateful
               (: name . type)
               (define (name . args) maybe-result body ...))]
            #:grammar
            [(maybe-result (code:line)
                           (code:line : result-type))]]]]{
                                                                          
 Defines @racket[name] as a pure function. The @racket[define-pure/stateful]
 form relies on @racket[pure/stateful], and therefore allows the function to
 return a value containing @tech{stateful} functions. On the other hand,
 @racket[define-pure/stateless] relies on @racket[pure/stateless], and
 therefore only allows the return value to contain @tech{stateless} functions.

 Due to the way the function is defined, a regular separate type annotation of
 the form @racket[(: name type)] would not work (the function is first defined
 using a temporary variable, and @racket[name] is merely a
 @tech["rename transformer"
       #:doc '(lib "scribblings/reference/reference.scrbl")] for that temporary
 variable).

 It is therefore possible to express such a type annotation by placing both
 the type annotation and the definition within a @racket[define-pure/stateless]
 or @racket[define-pure/stateful] form:

 @racketblock[
 (define-pure/stateless
   (: square : (→ Number Number))
   (define (square x) (* x x)))]}

@(define-syntax (show-pure-ids stx)
   (with-syntax ([(id ...) (map (λ (id) (datum->syntax #'here (syntax-e id)))
                                (sort (free-id-set->list
                                       built-in-pure-functions-free-id-set)
                                      symbol<?
                                      #:key syntax-e))])
     #`(itemlist
        (item (list (racket id)))
        ...)))

@defthing[built-in-pure-functions-set (and/c generic-set? set-eq? set?)]{
 This set contains the built-in functions recognized as pure by this library.

 For now only a few built-in functions are recognized as pure:

 @(show-pure-ids)

 Patches adding new functions to the set are welcome.}

@defthing[#:kind "for-syntax value"
          built-in-pure-functions-free-id-set immutable-free-id-set?]{
 This value is provided at level 1, and contains the identifiers of the
 functions present in @racket[built-in-pure-functions-set].}

@defproc[((immutable/stateless/c [varref Variable-Reference]) [v Any]) Boolean]{
 Returns a predicate which accepts only values which are immutable, possibly
 containing @tech{stateless} functions, but not @tech{stateful} functions.

 This predicate detects whether the functions contained within the value
 @racket[v] are pure or not, based on the @racket[built-in-pure-functions-set]
 set and a few special cases:

 @itemlist[
 @item{The low-level functions used to build pure promises are always
   accepted. Their valid use is guaranteed by the macros wrapping them.}
 @item{Predicates for @racket[struct] types are always accepted}
 @item{Field accessors for @racket[struct] types are always accepted}
 @item{Constructors for @racket[struct] types are accepted only if
   @racket[immutable/stateless/c] can determine that the struct type is
   immutable.}]

 There seems to be no combination of built-in functions in Racket which would
 reliably associate a struct constructor (as a value) with its corresponding
 struct type. Instead, @racket[immutable/stateless/c] uses a heuristic based on
 @racket[object-name]: if @racket[struct-constructor-procedure?] returns
 @racket[#true] for a function, and that function's @racket[object-name] is
 @racket[st] or @racket[make-st], then @racket[st] is expected to be an
 identifier with static struct type information.

 To achieve this, it is necessary to access the call-site's namespace, which is
 done via the @racket[varref] parameter. Simply supplying the result of
 @racket[(#%variable-reference)] should be enough.}

@defproc[((immutable/stateful/c [varref Variable-Reference]) [v Any]) Boolean]{
 Returns a predicate which accepts only values which are immutable, possibly
 containing both @tech{stateful} and @tech{stateless} functions.

 This predicate needs to access the call-site's namespace, which is
 done via the @racket[varref] parameter. Simply supplying the result of
 @racket[(#%variable-reference)] should be enough.

 See the documentation for @racket[immutable/stateless/c] for an explanation
 of the reason for this need.}

@defform[(unsafe-pure/stateless expression)]{
 Indicates that the expression should be trusted as allowable within a
 @racket[pure/stateless] or @racket[pure/stateful] block or one of their
 derivatives. No check is performed on the expression.

 The @racket[unsafe-pure/stateless] form can be used within
 @racket[pure/stateless], @racket[pure/stateful] and their derivatives, to
 prevent any check on a portion of code.

 The expression should be a pure, stateless expression.
 
 Note that in the current implementation, the @racket[expression] is lifted
 (in the sense of @racket[syntax-local-lift-expression].}

@defform[(unsafe-operation/mutating expression)]{
 Indicates that the expression should be trusted as allowable within a
 @racket[pure/stateful] block, or one of its derivatives. No check is performed
 on the expression.

 The @racket[expression] should not vary its outputs and effects based on
 external state (i.e. its outputs and effects should depend only on the
 arguments passed to it).

 The @racket[expression] function may internally use mutation. It may return
 freshly-created stateful objects (closures over freshly-created mutable
 variables, closures over mutable arguments, and mutable data structure which
 are freshly created or extracted from the arguments). It may mutate any
 mutable data structure passed as an argument.

 Note that in the current implementation, the @racket[expression] is lifted
 (in the sense of @racket[syntax-local-lift-expression].}

@defform[(unsafe-declare-pure/stateless identifier)]{
 Declares that the given identifier should be trusted as a stateless pure
 function. The given function is subsequently treated like the functions
 present in @racket[built-in-pure-functions-set].

 Note that this has a global effect. For one-off exceptions, especially when
 it's not 100% clear whether the function is always pure and stateless, prefer
 @racket[unsafe-pure/stateless].}

@defform[(unsafe-declare-allowed-in-pure/stateful identifier)]{
 Declares that the given identifier should be trusted as a function that can
 be used within @racket[pure/stateful] and its derivatives.

 The @racket[identifier] function should not vary its outputs and effects
 based on external state (i.e. its outputs and effects should depend only on
 the arguments passed to it).

 The @racket[identifier] function may internally use mutation. It may return
 freshly-created stateful objects (closures over freshly-created mutable
 variables, closures over mutable arguments, and mutable data structure which
 are freshly created or extracted from the arguments). It may mutate any
 mutable data structure passed as an argument.

 Note that this has a global effect. For one-off exceptions, prefer
 @racket[unsafe-operation/mutating].}