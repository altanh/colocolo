#lang rosette

(require "../engine/skolemizer.rkt"
         "../lang/universe.rkt"
         "../lang/bounds.rkt"
         "../engine/interpretation.rkt"
         "../engine/engine.rkt"
         "../lang/ast.rkt")

(provide skolemize-merge)

(define (calculate-bound-tuple bnds new-bound)
  (match new-bound
    [(node/expr/op/-> arity children)
     (define upper-bounds (map (curry get-upper-bound bnds) children))
     (map (curry apply append) (apply cartesian-product upper-bounds))]
    [(node/expr/relation name r)
     (get-upper-bound bnds new-bound)]
    [_ (raise-argument-error
        'calculate-bound-tuple
        "cross product or relation" new-bound)]))

(define (skolemize-merge bnds formula skolem-depth)
  (let* ([formula-skolemized (skolemize formula skolem-depth #t)]
         [skolem-bnds
          (bounds-union
           bnds
           (bounds
            (bounds-universe bnds)
            (map
             (λ (pair)
               (make-upper-bound
                (car pair)
                (calculate-bound-tuple bnds (cdr pair))))
             (skolemized-bounds formula-skolemized))))])
    (begin (pretty-print (skolemized-formula formula-skolemized))
    (cons (skolemized-formula formula-skolemized) skolem-bnds))))