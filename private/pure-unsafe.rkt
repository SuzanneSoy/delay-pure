#lang racket

(require racket/promise
         racket/private/promise
         (for-syntax racket/base))
  
(provide (rename-out [promise/pure/stateful? promise/pure/maybe-stateful?])
         promise/pure/stateless?
         make-promise/pure/stateful
         make-promise/pure/stateless
         (struct-out declared-stateful-pure-function)
         (struct-out declared-stateless-pure-function))

(define-struct (promise/pure/stateful promise) ()
  #:property prop:force (λ(p) ((pref p))))

(define-struct (promise/pure/stateless promise/pure/stateful) ()
  #:property prop:force (λ(p) ((pref p))))

(define-struct declared-stateful-pure-function (f)
  #:property prop:procedure (struct-field-index f))

(define-struct
  (declared-stateless-pure-function declared-stateful-pure-function)
  ())