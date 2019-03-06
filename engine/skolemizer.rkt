#lang s-exp rosette

(require racket/require racket/hash (matching-identifiers-in #rx"^node/.+$" "../lang/ast.rkt")
         "../lang/ast.rkt"
         "../lang/bounds.rkt" "../lang/universe.rkt"
         "matrix.rkt" "matrix-ops.rkt" "symmetry.rkt" "interpretation.rkt"
         (prefix-in $ racket))

(provide (struct-out skolemized)
         skolemize)
; a skolemized formula defines new bounds on the new skolem relation that must be
; added to the problem : skolemized-bounds only
; contains new bounds, the caller is responsible for adding
; these decls to the problem bounds
; bounds : relation -> relation hash
; top-skolem-constraints : formula set (hashset)
; formula : formula
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
    [(node/expr/op _ _)
     (skolemize-expr-op formula rep-env skolem-depth non-skolems negated)]
    ; leaf node -- relation
    [(node/expr/relation arity name)
     (let ([formula (hash-ref rep-env formula formula)])
               (skolemized (make-immutable-hasheq) (seteq) formula))]
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
    [(node/formula/op _)
     (skolemize-formula-op formula rep-env skolem-depth non-skolems negated)]
    [(node/formula/quantified quantifier decls f)
       (skolemize-quantifier quantifier decls f rep-env skolem-depth non-skolems negated)]
    [(node/formula/multiplicity mult expr)
     (node/formula/multiplicity
      mult
      (skolemize-body formula rep-env -1 non-skolems negated))]))

(define (skolemize-expr-op formula rep-env skolem-depth non-skolems negated)
  (define (skolemize-children op formula)
    (let ([children
           (map
            (λ (child) (skolemize-body child rep-env skolem-depth non-skolems negated))
            (node/expr/op-children formula))])
      (skolemized
       (apply hash-union
              (map skolemized-bounds children))
       (apply set-union
              (map skolemized-top-skolem-constraints children))
       (op (node/expr-arity formula) (map (λ (child) (skolemized-formula child)) children)))))
  (match formula
    [(? node/expr/op/+?)
     (skolemize-children node/expr/op/+ formula)]
    [(? node/expr/op/&?)
     (skolemize-children node/expr/op/& formula)]
    [(? node/expr/op/-?)
     (skolemize-children node/expr/op/- formula)]
    [(? node/expr/op/->?)
     (skolemize-children node/expr/op/-> formula)]
    [(? node/expr/op/~?)
     (skolemize-children node/expr/op/~ formula)]
    [(? node/expr/op/join?)
     (skolemize-children node/expr/op/join formula)]
    [(? node/expr/op/^?)
     (skolemize-children node/expr/op/^ formula)]
    [(? node/expr/op/*?)
     (skolemize-children node/expr/op/* formula)]
    [(? node/expr/op/<:?)
     (skolemize-children node/expr/op/<: formula)]
    [(? node/expr/op/:>?)
     (skolemize-children node/expr/op/:> (skolemize-children formula))]
    [(? node/expr/op/ite?)
     (skolemize-children node/expr/op/ite formula)]))

(define (skolemize-formula-op formula rep-env skolem-depth non-skolems negated)
  (define (merge op children)
    (skolemized
       (apply hash-union
              (map skolemized-bounds children))
       (apply set-union
              (map skolemized-top-skolem-constraints children))
       (op (map (λ (child) (skolemized-formula child)) children))))
  (define (skolemize-children op formula skolem-depth)
    (let ([children
           (map
            (λ (child) (skolemize-body child rep-env skolem-depth non-skolems negated))
            (node/formula/op-children formula))])
      (merge op children)))
    (match formula
    [(? node/formula/op/=>?)
     (let* ([args (node/expr/op-children formula)]
            [left (car args)]
            [right (cadr args)])
       (merge node/formula/op/=>
         (list (skolemize-body left rep-env -1 non-skolems #f)
               (skolemize-body right rep-env -1 non-skolems negated))))]
    [(? node/formula/op/&&?)
     (skolemize-children node/formula/op/&& formula (if negated -1 skolem-depth))]
    [(? node/formula/op/||?)
     (skolemize-children node/formula/op/|| formula (if negated skolem-depth -1))]
    [(? node/formula/op/in?)
     (skolemize-children node/formula/op/in formula skolem-depth)]
    [(? node/formula/op/=?)
     (skolemize-children node/formula/op/= formula skolem-depth)]
    [(? node/formula/op/!?)
     (skolemize-children node/formula/op/! formula skolem-depth)]))

(define (skolemize-quantifier quantifier decls f rep-env skolem-depth non-skolems negated)
  (define (skolemize)
    ; create skolem relation
    ; calculate bounds on skolem relation
    ; join with nonSkolems to create skolem expression
    ; add skolem expression to repEnv
    ; return composition of range constraints and skolemized formula
    ; add domain constraints to top-skolem-constraints
    (let ([skolems
           (map (curryr create-skolem non-skolems) decls)])
      (let ([rep-env
             (for/fold
              ([rep-env rep-env])
              ([decl (map car decls)]
               [skolem skolems])
               (hash-set rep-env decl (skolem-expr skolem)))]
            [formula-skolemized
             (skolemize-body f rep-env skolem-depth non-skolems negated)])
        (skolemized
         (for/fold
          ([new-bounds (skolemized-bounds formula-skolemized)])
          ([skolem skolems])
           (hash-set new-bounds
                     (skolem-relation skolem)
                     (skolem-upper-bound skolem)))
         (set-union
                (skolemized-top-skolem-constraints formula-skolemized)
                (list->seteq (map (λ (skolem) (skolem-domain-constraint skolem)) skolems)))
         ((if (equal? 'all quantifier) -> &&)
          (apply && (map skolem-range-constraint skolems))
          (skolemized-formula formula-skolemized))))))
  (define (not-skolemize)
    ; check if possible to skolemize below, if not
    ; recurse with depth -1 otherwise:
    ; add variables in decls to nonSkolems
    ; decrement skolemDepth
    ; recurse on formula
    ; calculate
    (if (< skolem-depth (+ (length non-skolems) (length decls)))
        ; cannot skolemize below due to given skolem depth restriction
        (let
            ([formula-skolemized (skolemize-body f rep-env -1 non-skolems negated)])
          (struct-copy
           skolemized
           formula-skolemized
           [formula (node/formula/quantified quantifier decls (skolemized-formula formula-skolemized))]))
        (let*
            ([non-skolems (append decls non-skolems)]
             [formula-skolemized (skolemize-body f rep-env (sub1 skolem-depth) non-skolems negated)])
          (struct-copy
           skolemized
           formula-skolemized
           [formula (node/formula/quantified quantifier decls (skolemized-formula formula-skolemized))]))))
  (match quantifier
    ['all #:when (and negated (>= skolem-depth 0)) (skolemize)]
    ['some #:when (and (! negated) (>= skolem-depth 0)) (skolemize)]
    [_ (not-skolemize)]))

(define (create-skolem decl non-skolems)
  (match-let
      ([(cons decl-var decl-expr) decl])
    (let* ([relation (declare-relation (+ (relation-arity decl-var) (length non-skolems)))]
           [expr (foldl (lambda (non-skolem expr)
                          (join (car non-skolem) expr)) relation non-skolems)]
           [upper-bound
            ; TODO: append decl-expr?
            (-> (if (> (length non-skolems) 1)
                    (apply -> (map cdr non-skolems))
                    (cdar non-skolems))
                decl-expr)]
           [domain-constraint
            (in
             (apply join relation (make-list (relation-arity decl-var) univ))
             (comprehension non-skolems true))]
           [range-constraints (&& (in expr decl-expr) (one expr))])
      (skolem relation expr upper-bound domain-constraint range-constraints))))

(define (skolemize f skolem-depth)
  (skolemize-body f (make-immutable-hasheq) skolem-depth empty #f))

;(define base-env (make-immutable-hasheq))

;(define decls1 (list (cons (declare-relation 1) (declare-relation 1))))

;(define-sig Platform)
;(define-sig Man
 ; [ceiling : Platform]
  ;[floor   : Platform])

;(define (Above m n) (= (join m floor) (join n ceiling)))

;(skolemize (all ([m Man]) (some ([n Man]) (Above n m))) 1)

;(skolemize (all ([m Man]) (some ([n Man]) (Above m n))) 1)