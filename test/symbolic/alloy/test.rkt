#lang rosette

(require colocolo/lib/alloy)

(define (clear!)
  (clear-alloy!)
  (clear-state!))

(module+ test
  (dynamic-require "basic.rkt" #f)
  (clear!)
  (dynamic-require "ceilingsandfloors.rkt" #f)
  (clear!))