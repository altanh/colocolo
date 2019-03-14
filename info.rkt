#lang info

(define collection "colocolo")

(define pkg-desc "A solver for relational logic")
(define version "0.0.1")
(define pkg-authors '(james))

(define deps '("base"
               "rackunit-lib"
               "sandbox-lib"
               "rosette"))
(define build-deps '("scribble-lib" "racket-doc"))

(define scribblings '(("scribblings/colocolo.scrbl" () (experimental))))
