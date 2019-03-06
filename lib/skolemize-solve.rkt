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
     (pretty-print children)
     (define mapped (map (curry get-upper-bound bnds) children))
     (pretty-print mapped)
     (define ret (apply cartesian-product mapped))
     (define ret2 (map (curry apply append) ret))
     (pretty-print ret2)
     ret2]
    [(node/expr/relation name r)
     (get-upper-bound bnds new-bound)]
    [_ (raise-argument-error
        'calculate-bound-tuple
        "cross product or relation" new-bound)]))

(define (skolemize-merge bnds formula skolem-depth)
  (let* ([formula-skolemized (skolemize formula skolem-depth)]
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
    (pretty-print (skolemized-bounds formula-skolemized))
    (cons (skolemized-formula formula-skolemized) skolem-bnds)))