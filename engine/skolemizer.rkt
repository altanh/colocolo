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
(struct skolemized (bounds top-skolem-constraints formula)
  #:transparent)

(struct skolem (relation expr upper-bound domain-constraint range-constraint)
  #:transparent)
; probably will have to pass back up the bounds created for the skolems
; TODO : add relations argument to check if referenced relation in universe?
; TODO : use cache
; node -> relation? relation? hash -> int -> decl? list -> bool? -> node
(define (skolemize-body formula rep-env skolem-depth non-skolems negated)
  (match formula
    [(node/expr/op arity args)
     empty]
    ; leaf node -- relation
    [(node/expr/relation arity name)
     (hash-ref rep-env formula formula)]
    ; leaf node constant
    [(node/expr/constant arity type)
     formula]
    [(node/expr/comprehension arity decls f)
     (let ([rep-env
            (for/fold
             ([acc rep-env])
             ([decl-var (map car decls)])
              (hash-set acc decl-var decl-var))])
       (node/expr/comprehension
        arity
        ; TODO: replace skolemized variables in decl-exprs?
        decls
        (skolemize-body formula rep-env -1 non-skolems negated)))]
    [(node/formula/op args)
     empty]
    [(node/formula/quantified quantifier decls f)
       (skolemize-quantifier universe quantifier decls f)]
    [(node/formula/multiplicity mult expr)
     (node/formula/multiplicity
      mult
      (skolemize-body formula rep-env -1 non-skolems negated))]))

(define (skolemize-expr-op universe op args)
  (define (skolemize-children formula)
    (map
     (λ (child) (skolemize-body child rep-env skolem-depth non-skolems negated))
     (node/expr/op-children formula)))
  (match op
    [(? node/expr/op/+?)
     (node/expr/+ (skolemize-children formula))]
    [(? node/expr/op/&?)
     (node/expr/& (skolemize-children formula))]
    [(? node/expr/op/-?)
     (node/expr/- (skolemize-children formula))]
    [(? node/expr/op/->?)
     (node/expr/-> (skolemize-children formula))]
    [(? node/expr/op/~?)
     (node/expr/~ (skolemize-children formula))]
    [(? node/expr/op/join?)
     (node/expr/join (skolemize-children formula))]
    [(? node/expr/op/^?)
     (node/expr/^ (skolemize-children formula))]
    [(? node/expr/op/*?)
     (node/expr/* (skolemize-children formula))]
    [(? node/expr/op/<:?)
     (node/expr/<: (skolemize-children formula))]
    [(? node/expr/op/:>?)
     (node/expr/:> (skolemize-children formula))]
    [(? node/expr/op/ite?)
     (node/expr/ite (skolemize-children formula))]))

(define (skolemize-formula-op formula rep-env skolem-depth non-skolems negated)
  (define (skolemize-children skolem-depth formula)
    (map
     (λ (child) (skolemize-body child rep-env skolem-depth negated))
     (node/formula/op-children formula)))
  (match formula
    [(? node/formula/op/=>?)
     (let* ([args (node/expr/op-children formula)]
            [left (car args)]
            [right (cadr args)])
       (node/formula/op/=>
        (skolemize-body left rep-env -1 non-skolems #f)
        (skolemize-body right rep-env -1 non-skolems negated)))]
    [(? node/formula/&&?)
     (node/formula/&& (skolemize-children
      (if negated -1 skolem-depth) formula))]
    [(? node/formula/||?)
     (node/formula/|| (skolemize-children
      (if negated skolem-depth -1) formula))]
    [(? node/formula/in?)
     (node/formula/in (skolemize-children skolem-depth formula))]
    [(? node/formula/=?)
     (node/formula/= (skolemize-children skolem-depth formula))]
    [(? node/formula/!?)
     (node/formula/! (skolemize-children skolem-depth formula))]))

(define (skolemize-quantifier quantifier decls f repEnv skolem-depth non-skolems negated)
  (define (skolemize)
    ; create skolem relation
    ; calculate bounds on skolem relation
    ; join with nonSkolems to create skolem expression
    ; add skolem expression to repEnv
    ; return composition of range constraints and skolemized formula
    ; add domain constraints to topSkolemConstraints
    (let ([skolems
           (map (curryr create-skolem non-skolems) decls)])
      skolems))
  (define (not-skolemize)
    ; check if possible to skolemize below, if not
    ; recurse with depth -1 otherwise:
    ; add variables in decls to nonSkolems
    ; decrement skolemDepth
    ; recurse on formula
    ; calculate 
    (let
        ([skolem-depth (sub1 skolem-depth)]
         [nonSkolems (append decls non-skolems)])
    (node/formula/quantified
     quantifier decls
     (skolemize-body f repEnv skolem-depth nonSkolems negated))))
  (match quantifier
    ['all #:when negated (skolemize)]
    ['some #:when (! negated) (skolemize)]
    [_ (not-skolemize)]))

(define (create-skolem decl non-skolems)
  (match-let
      ([(cons decl-var decl-expr) decl])
    (let* ([relation (declare-relation (+ (relation-arity decl-var) (length non-skolems)))]
           [expr (foldl (lambda (non-skolem expr)
                          (join (car non-skolem) expr)) relation non-skolems)]
           [upper-bound
            ; TODO: append decl-expr?
            (-> (apply -> (map cdr non-skolems))
                decl-expr)]
           [domain-constraint
            (in
             (apply join relation (make-list (relation-arity decl-var) univ))
             (comprehension non-skolems true))]
           [range-constraints (&& (in expr decl-expr) (one expr))])
      (skolem relation expr upper-bound domain-constraint range-constraints))))

(define (add-to-rep-env rep-env decls non-skolems)
  (foldl (λ (decl rep-env)
           (match-let ([(cons var relation) decl])
             (hash-set rep-env var
                       (cons (declare-relation (+ (relation-arity var) (length non-skolems)))
                             relation)))) ; probably have to union with nonSkolems or something
         rep-env
         decls))


(define (skolemize f skolem-depth)
  (skolemize-body f (make-immutable-hasheq) skolem-depth empty #f))

(define base-env (make-immutable-hasheq))

(define decls1 (list (cons (declare-relation 1) (declare-relation 1))))