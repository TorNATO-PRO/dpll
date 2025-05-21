#lang typed/racket

;; This module defines the core types and operations for representing
;; Boolean formulas in Conjunctive Normal Form (CNF), as used in
;; SAT solving algorithms like DPLL and CDCL.

(provide
 ;; Types
 Var
 (struct-out Lit)
 Clause
 CNF
 Model
 
 ;; Constructors
 pos
 neg
 
 ;; Operations
 negate
 lit->string
 clause->string
 cnf->string
 
 ;; Model operations
 empty-model
 model-satisfied?
 model-assign
 model-lookup)

;; A variable is represented by a natural number
(define-type Var Natural)

;; A literal is either a positive or negated variable
(struct: Lit ([negated : Boolean] [var : Var]) #:transparent #:type-name Lit)

;; Create a positive literal from a variable
(: pos (-> Var Lit))
(define (pos variable)
  (Lit #f variable))  ;; Note: #f means NOT negated (positive)

;; Create a negative literal from a variable
(: neg (-> Var Lit))
(define (neg variable)
  (Lit #t variable))  ;; Note: #t means negated (negative)

;; Negate a literal
(: negate (-> Lit Lit))
(define (negate lit)
  (Lit (not (Lit-negated lit)) (Lit-var lit)))

;; A clause is a disjunction of literals
(define-type Clause (Listof Lit))

;; A CNF formula is a conjunction of clauses
(define-type CNF (Listof Clause))

;; A model/assignment maps variables to Boolean values
(define-type Model (HashTable Var Boolean))

;; Create an empty model
(: empty-model (-> Model))
(define (empty-model)
  (make-hash))

;; Check if a literal is satisfied by a model
(: lit-satisfied? (-> Lit Model (Option Boolean)))
(define (lit-satisfied? lit model)
  (define var-assignment (hash-ref model (Lit-var lit) #f))
  (cond
    [(not var-assignment) #f]  ;; Variable not assigned
    [else (if (Lit-negated lit)
              (not var-assignment)
              var-assignment)]))

;; Check if a clause is satisfied by a model
(: clause-satisfied? (-> Clause Model Boolean))
(define (clause-satisfied? clause model)
  (ormap (λ ([lit : Lit]) 
           (case (lit-satisfied? lit model)
             [(#t) #t]
             [else #f]))
         clause))

;; Check if a CNF formula is satisfied by a model
(: model-satisfied? (-> CNF Model Boolean))
(define (model-satisfied? cnf model)
  (andmap (λ ([clause : Clause]) (clause-satisfied? clause model)) cnf))

;; Assign a value to a variable in a model
(: model-assign (-> Model Var Boolean Model))
(define (model-assign model var value)
  (hash-set model var value))

;; Look up a variable's value in a model
(: model-lookup (-> Model Var (Option Boolean)))
(define (model-lookup model var)
  (hash-ref model var #f))

;; Convert a literal to a string
(: lit->string (-> Lit String))
(define (lit->string lit)
  (format "~a~a" 
          (if (Lit-negated lit) "¬" "")
          (Lit-var lit)))

;; Convert a clause to a string
(: clause->string (-> Clause String))
(define (clause->string clause)
  (format "(~a)" 
          (string-join (map lit->string clause) " ∨ ")))

;; Convert a CNF formula to a string
(: cnf->string (-> CNF String))
(define (cnf->string cnf)
  (string-join (map clause->string cnf) " ∧ "))
