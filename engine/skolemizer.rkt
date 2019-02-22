#lang s-exp rosette

(require racket/require (matching-identifiers-in #rx"^node/.+$" "../lang/ast.rkt")
         (only-in "../lang/ast.rkt" relation-name relation-arity)
         "../lang/bounds.rkt" "../lang/universe.rkt"
         "matrix.rkt" "matrix-ops.rkt" "symmetry.rkt" "interpretation.rkt"
         (prefix-in $ racket))

; probably will have to pass back up the bounds created for the skolems
; node -> relation? list -> relation? relation? hash -> int -> decl? list -> bool -> cache -> node
(define (skolemize-body formula relations repEnv skolemDepth nonSkolems negated cache)
  (match formula
    [(node/expr/op arity args)
     (let ([args* (for/list ([a (in-list args)]) (interpret-rec a universe relations cache))])
       (interpret-expr-op universe formula args*))]
    ; leaf node -- relation
    [(node/expr/relation arity name)
     (hash-ref repEnv formula formula)]
    ; leaf node constant
    [(node/expr/constant arity type)
     formula]
    [(node/expr/comprehension arity decls f)
     (let ([decls* (for/list ([d (in-list decls)]) (cons (car d) (interpret-rec (cdr d) universe relations cache)))])
       (interpret-comprehension universe relations decls* f cache))]
    [(node/formula/op args)
     (interpret-formula-op universe relations formula args cache)]
    [(node/formula/quantified quantifier decls f)
       (skolemize-quantifier universe relations quantifier decls* f cache)]
    [(node/formula/multiplicity mult expr)
     (let ([expr* (interpret-rec expr universe relations cache)])
       (interpret-multiplicity universe mult expr*))]))

(define (skolemize-quantifier quantifier decls f relations repEnv skolemDepth nonSkolems negated cache)
  (case quantifier
    ; add variables in decls to nonSkolems
    ; decrement skolemDepth
    ; recurse on formula
    ['all (let ([skolemDepth (- skolemDepth 1)]
                [nonSkolems (append decls nonSkolems)])
            (node/formula/quantified quantifier decls (skolemize-body f relations repEnv skolemDepth nonSkolems negated cache)]
    ; add skolems to repEnv
    ; recurse on formula
    ['some (let ([repEnv ()])
             (skolemize-body f relations repEnv skolemDepth nonSkolems negated cache))]))