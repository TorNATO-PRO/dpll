#lang typed/racket

;; Result.rkt - A complete Result type implementation for Typed Racket
;; Provides a type-safe way to represent and handle errors,
;; similar to Either in Haskell or Result in Rust/Scala/OCaml.

(provide
 ;; Types
 (struct-out Err)
 (struct-out Ok)
 Result
 
 ;; Constructors 
 err
 ok
 result-pure
 
 ;; Predicates
 result-err?
 result-ok?
 
 ;; Unwrapping
 result-unwrap
 result-unwrap-or
 result-unwrap-or-else
 result-err-or
 
 ;; Mapping
 result-map
 result-map-ok
 result-map-err
 
 ;; Applicative and Monadic operations
 result-apply
 result-bind
 result-sequence
 
 ;; Combinators
 result-alt
 result-and
 result-or
 result-try
 
 ;; Do notation
 result-do)

;; Define the Result type variants with transparent structs
(struct: (A) Err ([value : A]) #:transparent #:type-name Err)
(struct: (B) Ok ([value : B]) #:transparent #:type-name Ok)

;; Define the Result type as a union of Err and Ok
(define-type (Result A B) (U (Err A) (Ok B)))

;; Construct an Err value with the proper type parameter
(: err (All (A B) (-> A (Result A B))))
(define (err a)
  (Err a))

;; Construct an Ok value with the proper type parameter
(: ok (All (A B) (-> B (Result A B))))
(define (ok b)
  (Ok b))

;; Check if a result is an Err
(: result-err? (All (A B) (-> (Result A B) Boolean)))
(define (result-err? res)
  (match res
    [(Err _) #t]
    [_ #f]))

;; Check if a result is Ok
(: result-ok? (All (A B) (-> (Result A B) Boolean)))
(define (result-ok? res)
  (match res
    [(Ok _) #t]
    [_ #f]))

;; Unwrap a result, returning the Ok value or raising an error with the given message
(: result-unwrap (All (A B) (-> (Result A B) String B)))
(define (result-unwrap res msg)
  (match res
    [(Ok v) v]
    [(Err e) (error msg)]))

;; Unwrap a result, returning the Ok value or using the supplied default
(: result-unwrap-or (All (A B) (-> (Result A B) B B)))
(define (result-unwrap-or res default)
  (match res
    [(Ok v) v]
    [(Err _) default]))

;; Unwrap a result, returning the Ok value or calling the function with the error value
(: result-unwrap-or-else (All (A B) (-> (Result A B) (-> A B) B)))
(define (result-unwrap-or-else res f)
  (match res
    [(Ok v) v]
    [(Err e) (f e)]))

;; Get the error value if present, or the default
(: result-err-or (All (A B) (-> (Result A B) A A)))
(define (result-err-or res default)
  (match res
    [(Err e) e]
    [(Ok _) default]))

;; Map a function over an Result's Ok value
(: result-map-ok (All (A B C) (-> (Result A B) (-> B C) (Result A C))))
(define (result-map-ok res f)
  (match res
    [(Ok v) (Ok (f v))]
    [(Err e) (Err e)]))

;; Map a function over an Result's Err value
(: result-map-err (All (A B C) (-> (Result A B) (-> A C) (Result C B))))
(define (result-map-err res f)
  (match res
    [(Ok v) (Ok v)]
    [(Err e) (Err (f e))]))

;; Map a function over a Result (alias for result-map-ok)
(: result-map (All (A B C) (-> (Result A B) (-> B C) (Result A C))))
(define (result-map res f)
  (result-map-ok res f))

;; Apply a function wrapped in an Result to a value wrapped in an Result
(: result-apply (All (A B C) (-> (Result A (-> B C)) (Result A B) (Result A C))))
(define (result-apply f res)
  (match f
    [(Err e) (Err e)]
    [(Ok fn) (result-map res fn)]))

;; Lift a value into a Result (same as Ok)
(: result-pure (All (A B) (-> B (Result A B))))
(define (result-pure b)
  (Ok b))

;; Return the first Ok value, or Err if both are Err
(: result-alt (All (A B) (-> (Result A B) (Result A B) (Result A B))))
(define (result-alt result-left result-right)
  (match result-left
    [(Ok v) result-left]
    [(Err _) result-right]))

;; Monadic bind operation - apply a function that returns a Result
(: result-bind (All (A B C) (-> (Result A B) (-> B (Result A C)) (Result A C))))
(define (result-bind result-left result-fn)
  (match result-left
    [(Err e) (Err e)]
    [(Ok a) (result-fn a)]))

;; Sequence a list of Results into a Result of list
(: result-sequence (All (A B) (-> (Listof (Result A B)) (Result A (Listof B)))))
(define (result-sequence results)
  (for/fold ([acc : (Result A (Listof B)) (ok '())])
            ([res (reverse results)])
    (result-bind res 
                (λ ([v : B])
                  (result-map acc (λ ([lst : (Listof B)]) (cons v lst)))))))

;; Try to apply function and catch exceptions as Err
(: result-try (All (A B) (-> (-> B) A (Result A B))))
(define (result-try f err-val)
  (with-handlers ([exn:fail? (λ (_) (Err err-val))])
    (Ok (f))))

;; And operation for Results
(: result-and (All (A B C) (-> (Result A B) (Result A C) (Result A C))))
(define (result-and res1 res2)
  (match res1
    [(Ok _) res2]
    [(Err e) (Err e)]))

;; Or operation for Results
(: result-or (All (A B) (-> (Result A B) (Result A B) (Result A B))))
(define (result-or res1 res2)
  (match res1
    [(Ok v) res1]
    [(Err _) res2]))

;; Helper macro to process the lines of a do-block
(define-syntax (process-do-lines stx)
  (syntax-case stx ()
    ;; Base case: last expression
    [(_ () last-expr)
     #'last-expr]
    
    ;; Binding expression: [var <- expr] rest ...
    [(_ ([var <- expr] rest ...) last-expr)
     #'(result-bind expr
                   (λ (var)
                     (process-do-lines (rest ...) last-expr)))]
    
    ;; Expression without binding: expr rest ...
    [(_ (expr rest ...) last-expr)
     #'(result-bind expr
                   (λ (_)
                     (process-do-lines (rest ...) last-expr)))]))

;; The result-do macro
(define-syntax (result-do stx)
  (syntax-case stx ()
    [(_ expr)
     #'expr]
    [(_ line ... last-expr)
     #'(process-do-lines (line ...) last-expr)]))
