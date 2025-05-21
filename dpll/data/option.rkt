#lang typed/racket

;; Option.rkt - A complete Option type implementation for Typed Racket
;; Provides a type-safe way to represent values that may or may not exist,
;; similar to Maybe in Haskell or Option in Rust/Scala/OCaml.

(provide
 ;; Types
 (struct-out Some)
 (struct-out None)
 Option

 ;; Core Constructors
 some
 none
 
 ;; Predicates
 option-some?
 option-none?
 
 ;; Transformations
 option-map
 option-apply
 option-pure
 option-bind
 option-join
 option-filter
 
 ;; Combinations
 option-alt
 option-and
 option-or
 option-sequence
 
 ;; Extractions
 option-get-or-else
 option-get-or-compute
 option-to-list
 option-to-vector
 
 ;; Utilities
 option-empty
 option-for-each

 ;; Do Notation
 option-do)

;; Define the Option type variants with transparent structs
(struct: (A) Some ([value : A]) #:transparent #:type-name Some)
(struct: None () #:transparent #:type-name None)

;; Define the Option type as a union of Some and None
(define-type (Option A) (U (Some A) None))

;; Create a singleton None instance to be used across the program
(: none-instance None)
(define none-instance (None))

;; Construct a None value with the proper type parameter
(: none (All (A) (-> (Option A))))
(define (none)
  none-instance)

;; Construct a Some value
(: some (All (A) (-> A (Option A))))
(define (some x)
  (Some x))

;; Check if an option is a Some
(: option-some? (All (A) (-> (Option A) Boolean)))
(define (option-some? opt)
  (match opt
    [(Some _) #t]
    [_ #f]))

;; Check if an option is None
(: option-none? (All (A) (-> (Option A) Boolean)))
(define (option-none? opt)
  (match opt
    [(None) #t]
    [_ #f]))

;; Map a function over an Option
(: option-map (All (A B) (-> (Option A) (-> A B) (Option B))))
(define (option-map opt f)
  (match opt
    [(Some v) (some (f v))]
    [(None) (none)]))

;; Apply a function wrapped in an Option to a value wrapped in an Option
(: option-apply (All (A B) (-> (Option (-> A B)) (Option A) (Option B))))
(define (option-apply opt-f opt)
  (match* (opt-f opt)
    [((Some f) (Some v)) (some (f v))]
    [(_ _) (none)]))

;; Lift a value into an Option (same as some)
(: option-pure (All (A) (-> A (Option A))))
(define option-pure some)

;; Return the first Some value, or None if both are None
(: option-alt (All (A) (-> (Option A) (Option A) (Option A))))
(define (option-alt option-left option-right)
  (match option-left
    [(Some _) option-left]
    [(None) option-right]))

;; Return an empty Option (same as none)
(: option-empty (All (A) (-> (Option A))))
(define option-empty none)

;; Monadic bind operation - apply a function that returns an Option
(: option-bind (All (A B) (-> (Option A) (-> A (Option B)) (Option B))))
(define (option-bind opt f)
  (match opt
    [(Some v) (f v)]
    [(None) (none)]))

;; Extract value or return default
(: option-get-or-else (All (A) (-> (Option A) A A)))
(define (option-get-or-else opt default)
  (match opt
    [(Some v) v]
    [(None) default]))

;; Extract value or compute default using a function
(: option-get-or-compute (All (A) (-> (Option A) (-> A) A)))
(define (option-get-or-compute opt default-fn)
  (match opt
    [(Some v) v]
    [(None) (default-fn)]))

;; Convert Option to a list (empty list or singleton list)
(: option-to-list (All (A) (-> (Option A) (Listof A))))
(define (option-to-list opt)
  (match opt
    [(Some v) (list v)]
    [(None) '()]))

;; Convert Option to a vector (empty vector or singleton vector)
(: option-to-vector (All (A) (-> (Option A) (Vectorof A))))
(define (option-to-vector opt)
  (match opt
    [(Some v) (vector v)]
    [(None) (vector)]))

;; Flatten a nested Option
(: option-join (All (A) (-> (Option (Option A)) (Option A))))
(define (option-join opt)
  (match opt
    [(Some (Some v)) (some v)]
    [_ (none)]))

;; Filter an option based on a predicate
(: option-filter (All (A) (-> (Option A) (-> A Boolean) (Option A))))
(define (option-filter opt pred)
  (match opt
    [(Some v) (if (pred v) opt (none))]
    [(None) (none)]))

;; Perform side effect on value if present
(: option-for-each (All (A) (-> (Option A) (-> A Void) Void)))
(define (option-for-each opt f)
  (match opt
    [(Some v) (f v)]
    [(None) (void)]))

;; Return Some only if both inputs are Some
(: option-and (All (A B) (-> (Option A) (Option B) (Option B))))
(define (option-and opt1 opt2)
  (match opt1
    [(Some _) opt2]
    [(None) (none)]))

;; Boolean OR semantics for Options
(: option-or (All (A) (-> (Option A) (Option A) (Option A))))
(define option-or option-alt)

;; Sequence a list of Options into an Option of list
;; Returns Some with a list of all values if all are Some, None otherwise
(: option-sequence (All (A) (-> (Listof (Option A)) (Option (Listof A)))))
(define (option-sequence opts)
  (let: loop : (Option (Listof A)) ([opts : (Listof (Option A)) opts]
                                    [result : (Listof A) '()])
    (cond
      [(null? opts) (some (reverse result))]
      [else
       (match (car opts)
         [(Some v) (loop (cdr opts) (cons v result))]
         [(None) (none)])])))

;; Do notation for the Option Monad

;; Helper macro to process the lines of a do-block
(define-syntax (process-do-lines stx)
  (syntax-case stx ()
    ;; Base case: last expression
    [(_ () last-expr)
     #'last-expr]
    
    ;; Binding expression: [var <- expr] rest ...
    [(_ ([var <- expr] rest ...) last-expr)
     #'(option-bind expr
                   (λ (var)
                     (process-do-lines (rest ...) last-expr)))]
    
    ;; Expression without binding: expr rest ...
    [(_ (expr rest ...) last-expr)
     #'(option-bind expr
                   (λ (_)
                     (process-do-lines (rest ...) last-expr)))]))


;; The option-do macro
(define-syntax (option-do stx)
  (syntax-case stx ()
    [(_ expr)
     #'expr]
    [(_ line ... last-expr)
     #'(process-do-lines (line ...) last-expr)]))
