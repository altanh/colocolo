#lang rosette

(require (prefix-in $ racket))
(require "translator.rkt")

(provide (all-defined-out))

(define solver-path "lingeling")
(define solver-args "-q")

(struct SAT (translator clauses) #:mutable)

(define (make-SAT)
  (SAT (make-translator) empty))

(define (SAT-assert SAT formula)
  (define cache (make-hash))
  (define root-value
    (visit (SAT-translator SAT) formula cache))
  (define new-clauses
    (cons (list (boolean/value-label root-value))
          (apply append (hash-values cache))))
  (set-SAT-clauses! SAT (append new-clauses (SAT-clauses SAT))))

(define (SAT-solve SAT [formulas empty])
  (for ([formula formulas]) (SAT-assert SAT formula))
  ;(define temp-path (make-temporary-file "colocolo~a.cnf"))
  ;(define temp-file (open-output-file temp-path #:exists 'truncate))
  ;(SAT-write SAT temp-file)
  ;(close-output-port temp-file)
  (match-define (list pout pin pid perr psig)
    (process* solver-path solver-args))
  (SAT-write SAT pin)
  (close-output-port pin)
  (psig 'wait)
  (define sol (read-solution pout))
  (close-input-port pout)
  (close-input-port perr)
  (if sol (make-model SAT sol) (unsat)))

(define (make-model SAT model-list)
  (sat
   ($for/hash ([assignment model-list]
              #:when (lookup-constant (SAT-translator SAT) (abs assignment)))
     (define key (lookup-constant (SAT-translator SAT) (abs assignment)))
     (values key (if (< assignment 0) #f #t)))))

(define (read-solution port [model empty])
  (define line (read-line port 'any))
  (cond
    [(eof-object? line) model]
    [(equal? (string-ref line 0) #\c) (read-solution port model)]
    [(equal? (string-ref line 0) #\s)
     (if (equal? (substring line 2) "SATISFIABLE")
         (read-solution port model)
         #f)]
    [(equal? (string-ref line 0) #\v)
     (define next-model
       (read (open-input-string (string-append "(" (substring line 2) ")"))))
     (read-solution port (append model next-model))]))

; translates the clauses in SAT into DIMACS CNF and writes to port
(define (SAT-write SAT port)
  (define num-var
    (sub1 (factory-next (translator-factory (SAT-translator SAT)))))
  (define num-clause (length (SAT-clauses SAT)))
  (printf "CNF-SAT instance has ~a vars and ~a clauses\n" num-var num-clause)
  (printf "\toriginally had ~a constants\n" (length (hash-keys (translator-constants (SAT-translator SAT)))))
  (fprintf port "p cnf ~a ~a\n" num-var num-clause)
  (for ([clause (SAT-clauses SAT)])
    (for ([label clause])
      (fprintf port "~a " label))
    (fprintf port "0\n")))
