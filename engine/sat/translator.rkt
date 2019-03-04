#lang rosette

(require (prefix-in $ racket))
(require rosette/base/core/term rosette/base/core/bool)

(provide (all-defined-out))

(struct factory (next) #:mutable)
(struct translator (factory cache constants) #:mutable #:transparent)
(struct boolean/value (label) #:transparent)
(struct boolean/var boolean/value () #:transparent)
(struct boolean/&& boolean/value () #:transparent)
(struct boolean/|| boolean/value () #:transparent)
(struct boolean/! boolean/value () #:transparent)

(define (make-factory) (factory 1))
(define (factory-next! factory)
  (define ret (factory-next factory))
  (set-factory-next! factory (add1 (factory-next factory)))
  ret)

(define (make-translator)
  (translator (make-factory) (make-hash) (make-hash)))
(define (translator-ref T formula)
  ($hash-ref (translator-cache T) formula))
(define (translator-set! T formula val)
  ($hash-set! (translator-cache T) formula val))
(define (translator-cached? T formula)
  ($hash-has-key? (translator-cache T) formula))
(define (translator-next! T)
  (factory-next! (translator-factory T)))
(define (lookup-constant T label)
  (hash-ref (translator-constants T) label #f))

(define (cache-clause! cache val clause)
  (define old-clauses (hash-ref! cache val empty))
  (hash-set! cache val (cons clause old-clauses)))
(define (cache-clauses! cache val clauses)
  (define old-clauses (hash-ref! cache val empty))
  (hash-set! cache val (append clauses old-clauses)))

(define (clauses/|| arg-labels label)
  (cons
   (cons (- label) (for/list ([arg-label arg-labels]) arg-label))
   (for/list ([arg-label arg-labels]) (list (- arg-label) label))))

(define (clauses/&& arg-labels label)
  (cons
   (cons label (for/list ([arg-label arg-labels]) (- arg-label)))
   (for/list ([arg-label arg-labels]) (list arg-label (- label)))))

(define (visit T formula cache)
  (match formula
    [(expression (== @||) args ...)
     (unless (translator-cached? T formula)
       (for ([arg args]) (visit T arg cache))
       (define val (boolean/|| (translator-next! T)))
       (translator-set! T formula val))
     (when cache
       (define arg-labels
         (map (compose boolean/value-label (curry translator-ref T)) args))
       (define val (translator-ref T formula))
       (define label (boolean/value-label val))
       (cache-clauses! cache val (clauses/|| arg-labels label)))
     (translator-ref T formula)]
    [(expression (== @&&) args ...)
     (unless (translator-cached? T formula)
       (for ([arg args]) (visit T arg cache))
       (define val (boolean/&& (translator-next! T)))
       (translator-set! T formula val))
     (when cache
       (define arg-labels
         (map (compose boolean/value-label (curry translator-ref T)) args))
       (define val (translator-ref T formula))
       (define label (boolean/value-label val))
       (cache-clauses! cache val (clauses/&& arg-labels label)))
     (translator-ref T formula)]
    [(expression (== @!) a)
     (unless (translator-cached? T formula)
       (define val (boolean/! (- (boolean/value-label (visit T a cache)))))
       (translator-set! T formula val))
     (translator-ref T formula)]
    [(expression (== @=>) a b)
     (unless (translator-cached? T formula)
       (define val (visit T (|| (! a) b) cache))
       (translator-set! T formula val))
     (translator-ref T formula)]
    [(expression (== @<=>) a b)
     (unless (translator-cached? T formula)
       (define val (visit T (|| (&& a b) (&& (! a) (! b))) cache))
       (translator-set! T formula val))
     (translator-ref T formula)]
    [(? constant?)
     (unless (translator-cached? T formula)
       (define label (translator-next! T))
       (hash-set! (translator-constants T) label formula)
       (translator-set! T formula (boolean/var label)))
     (translator-ref T formula)]
    [id (raise-argument-error 'visit "boolean? formula" formula)]))