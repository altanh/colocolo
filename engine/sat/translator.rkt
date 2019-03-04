#lang rosette

(require (prefix-in $ racket))
(require rosette/base/core/term rosette/base/core/bool)

(struct factory (next) #:mutable)
(struct translator (factory cache clauses) #:mutable #:transparent)
(struct boolean/value (label) #:transparent)
(struct boolean/&& boolean/value () #:transparent)
(struct boolean/|| boolean/value () #:transparent)
(struct boolean/! boolean/value () #:transparent)

(define (hash-formula formula)
  ($eq-hash-code formula))

(define (make-factory) (factory 1))
(define (factory-next! factory)
  (define ret (factory-next factory))
  (set-factory-next! factory (add1 (factory-next factory)))
  ret)

(define (make-translator)
  (translator (make-factory) (make-hash) empty))
(define (translator-ref T formula)
  ($hash-ref (translator-cache T) formula))
(define (translator-set! T formula val)
  ($hash-set! (translator-cache T) formula val))
(define (translator-cached? T formula)
  ($hash-has-key? (translator-cache T) formula))
(define (translator-next! T)
  (factory-next! (translator-factory T)))
(define (add-clause! T clause)
  (set-translator-clauses! T (cons clause (translator-clauses T))))
(define (add-clauses! T clauses)
  (set-translator-clauses! T (append clauses (translator-clauses T))))

(define-symbolic* a b boolean?)
(define formula
  (=> a (|| (! a) b)))

(define (visit T formula)
  (match formula
    [(expression (== @||) args ...)
     (unless (translator-cached? T formula)
       (define arg-vals
         (map (compose boolean/value-label (curry visit T)) args))
       (define val (boolean/&& (translator-next! T)))
       (define label (boolean/value-label val))
       (define clauses
         (cons
          (cons (- label) (for/list ([arg-val arg-vals]) arg-val))
          (for/list ([arg-val arg-vals]) (list (- arg-val) label))))
       (add-clauses! T clauses)
       (translator-set! T formula val))
     (translator-ref T formula)]
    [(expression (== @&&) args ...)
     (unless (translator-cached? T formula)
       (define arg-vals
         (map (compose boolean/value-label (curry visit T)) args))
       (define val (boolean/&& (translator-next! T)))
       (define label (boolean/value-label val))
       (define clauses
         (cons
          (cons label (for/list ([arg-val arg-vals]) (- arg-val)))
          (for/list ([arg-val arg-vals]) (list arg-val (- label)))))
       (add-clauses! T clauses)
       (translator-set! T formula val))
     (translator-ref T formula)]
    [(expression (== @!) a)
     (unless (translator-cached? T formula)
       (define val (boolean/! (- (boolean/value-label (visit T a)))))
       (translator-set! T formula val))
     (translator-ref T formula)]
    [(expression (== @=>) a b)
     (visit T (|| (! a) b))]
    [(? constant?)
     (unless (translator-cached? T formula)
       (define val (boolean/value (translator-next! T)))
       (translator-set! T formula val))
     (translator-ref T formula)]
    [id (raise-argument-error 'visit "boolean formula" formula)]))

(define (translator-assert T formula)
  (visit T formula)
  (add-clause! T (list (boolean/value-label (translator-ref T formula)))))

(define T (make-translator))
(translator-assert T formula)
