#lang typed/racket

; This module provides a recursive descent parser for formulas in the DIMACS CNF format.
;
; A DIMACS CNF file has the following structure:
;
; - Zero or more comment lines beginning with 'c'
; - A preamble line beginning with 'p', of the form:
;       p cnf <num_vars> <num_clauses>
; - A sequence of integers representing clauses, where:
;     - Each clause is a sequence of space-separated integers (literals)
;     - A clause *must* be terminated by the integer 0
;     - Clauses may span multiple lines; the only delimiter is the 0
;     - Positive integers denote positive literals; negative integers denote negated literals
;     - Variables are numbered from 1 to <num_vars>
;
; Example:
;   c This is a comment
;   p cnf 3 2
;   1 -3 0
;   2 3 -1 0
;
; This represents the formula: (x1 ∨ ¬x3) ∧ (x2 ∨ x3 ∨ ¬x1)
;
; Here is the rough grammar of DIMACS.
;
; dimacs ::= comments? preamble clauses
; comments ::= comment-line*
; comment-line ::= 'c' .* '\n'
;
; preamble ::= 'p' 'cnf' num-vars num-clauses '\n'
;
; clauses ::= clause*
; clause ::= literal+ '0'
;
; literal ::= INT /= 0
; num-vars ::= INT > 0
; num-clauses ::= INT >= 0

;; Type for the input buffer
(define-type CharBuffer (Listof Char))

;; Type for parser results with various payload types
(define-type (ParseResult A) (Pair CharBuffer (Option A)))

;; Helper function to check if a character is whitespace
(: whitespace? (-> Char Boolean))
(define (whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

;; Helper function to skip whitespace
(: skip-whitespace (-> CharBuffer CharBuffer))
(define (skip-whitespace input)
  (if (or (null? input)
          (not (whitespace? (car input))))
      input
      (skip-whitespace (cdr input))))

;; Helper function to read until end of line
(: read-until-eol (-> CharBuffer CharBuffer))
(define (read-until-eol input)
  (cond ((null? input) '())
        ((char=? (car input) #\newline) (cdr input))
        (else (read-until-eol (cdr input)))))

