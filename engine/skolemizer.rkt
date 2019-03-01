#lang s-exp rosette

(require racket/require (matching-identifiers-in #rx"^node/.+$" "../lang/ast.rkt")
         "../lang/ast.rkt"
         "../lang/bounds.rkt" "../lang/universe.rkt"
         "matrix.rkt" "matrix-ops.rkt" "symmetry.rkt" "interpretation.rkt"
         (prefix-in $ racket))
; a skolemized formula defines new bounds on the new skolem relation that must be
; added to the problem : skolemized-bounds only
; contains new bounds, the caller is responsible for adding
; these decls to the problem bounds
(struct skolemized (bounds topSkolemConstraints formula))

(struct skolem (relation expr upper-bound domain range))
; probably will have to pass back up the bounds created for the skolems
; TODO : add relations argument to check if referenced relation in universe?
; TODO : use cache
; node -> relation? relation? hash -> int -> decl? list -> bool? -> node
(define (skolemize-body formula repEnv skolemDepth nonSkolems negated)
  (match formula
    [(node/expr/op arity args)
     empty]
    ; leaf node -- relation
    [(node/expr/relation arity name)
     (hash-ref repEnv formula formula)]
    ; leaf node constant
    [(node/expr/constant arity type)
     formula]
    [(node/expr/comprehension arity decls f)
     empty]
    [(node/formula/op args)
     empty]
    [(node/formula/quantified quantifier decls f)
       (skolemize-quantifier universe quantifier decls f)]
    [(node/formula/multiplicity mult expr)
     empty]))


(define (skolemize-quantifier quantifier decls f repEnv skolemDepth nonSkolems negated)
  (define (skolemize)
    ; create skolem relation
    ; calculate bounds on skolem relation
    ; join with nonSkolems to create skolem expression
    ; add skolem expression to repEnv
    ; return composition of range constraints and skolemized formula
    ; add domain constraints to topSkolemConstraints
    (let ([skolems
           (map (λ (decl)
                  (create-skolem decl nonSkolems))
                decls)])
      skolems))
  (define (not-skolemize)
    ; check if possible to skolemize below, if not
    ; recurse with depth -1 otherwise:
    ; add variables in decls to nonSkolems
    ; decrement skolemDepth
    ; recurse on formula
    ; calculate 
    (let
        ([skolemDepth (sub1 skolemDepth)]
         [nonSkolems (append decls nonSkolems)])
    (node/formula/quantified
     quantifier decls
     (skolemize-body f repEnv skolemDepth nonSkolems negated))))
  (match quantifier
    ['all #:when negated (skolemize)]
    ['some #:when (! negated) (skolemize)]
    [_ (not-skolemize)]))

(define (create-skolem decl nonSkolems)
  (let ([relation (match-let ([(cons var relation) decl])
                    (declare-relation (+ (relation-arity var) (length nonSkolems))))])
    (let ([expr empty]
          [upper-bound empty]
          [domain-constraints empty]
          [range-constraints empty])
      (skolem relation expr upper-bound domain-constraints range-constraints))))

(define (add-to-repEnv repEnv decls nonSkolems)
  (foldl (λ (decl repEnv)
           (match-let ([(cons var relation) decl])
             (hash-set repEnv var
                       (cons (declare-relation (+ (relation-arity var) (length nonSkolems)))
                             relation)))) ; probably have to union with nonSkolems or something
         repEnv
         decls))


(define (skolemize f skolemDepth)
  (skolemize-body f (make-immutable-hasheq) skolemDepth empty #f))

(define base-env (make-immutable-hasheq))

(define decls1 (list (cons (declare-relation 1) (declare-relation 1))))