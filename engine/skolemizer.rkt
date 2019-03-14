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

(define (lookup f f-thunk cache)
  ($if cache
       (hash-ref! cache f f-thunk)
       (f-thunk)))
; probably will have to pass back up the bounds created for the skolems
; TODO : add relations argument to check if referenced relation in universe?
; TODO : use cache
; node -> relation? relation? hash -> int -> decl? list -> bool? -> node
(define (skolemize-body formula rep-env skolem-depth non-skolems negated cache)
  (match formula
    [(node/expr/op _ _)
     (lookup formula
             (thunk
              (skolemize-expr-op formula rep-env skolem-depth non-skolems negated cache))
             cache)]
    ; leaf node -- relation
    [(node/expr/relation arity name)
     (let ([formula (let ([found (assoc formula rep-env)])
                      (or (and found (cdr found)) formula))])
       (skolemized empty empty formula))]
    ; leaf node constant
    [(node/expr/constant arity type)
     (skolemized empty empty formula)]
    [(node/expr/comprehension arity decls f)
     (lookup formula
             (thunk (let ([rep-env
                           (for/fold
                            ([acc rep-env])
                            ([decl-var (map car decls)])
                             (cons (cons decl-var decl-var) acc))])
                      (node/expr/comprehension
                       arity
                       ; TODO: replace skolemized variables in decl-exprs?
                       decls
                       (skolemize-body formula rep-env -1 non-skolems negated cache))))
             cache)]
    [(node/formula/constant _)
     (skolemized empty empty formula)]
    [(node/formula/op _)
     (lookup formula
             (thunk
              (skolemize-formula-op formula rep-env skolem-depth non-skolems negated cache))
             cache)]
    [(node/formula/quantified quantifier decls f)
     (lookup formula
             (thunk
              (skolemize-quantifier quantifier decls f rep-env skolem-depth non-skolems negated cache))
             cache)]
    [(node/formula/multiplicity mult expr)
     (lookup formula
             (thunk
              (let ([expr-skolemized (skolemize-body expr rep-env -1 non-skolems negated cache)])
                (struct-copy
                 skolemized
                 expr-skolemized
                 [formula (node/formula/multiplicity
                           mult
                           (skolemized-formula expr-skolemized))])))
             cache)]))

(define (merge-bags . lists)
  (for/fold
   ([acc (car lists)])
   ([sublist (cdr lists)])
    (for/fold
     ([tail acc])
     ([el sublist])
      (cons el tail))))

(define (skolemize-expr-op formula rep-env skolem-depth non-skolems negated cache)
  (define (skolemize-children op formula)
    (let ([children
           (map
            (λ (child) (skolemize-body child rep-env skolem-depth non-skolems negated cache))
            (node/expr/op-children formula))])
      (skolemized
       (apply merge-bags
              (map skolemized-bounds children))
       (apply merge-bags
              (map skolemized-top-skolem-constraints children))
       (op (node/expr-arity formula) (map skolemized-formula children)))))
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

(define (skolemize-formula-op formula rep-env skolem-depth non-skolems negated cache)
  (define (merge op children)
    (skolemized
     (apply merge-bags
            (map skolemized-bounds children))
     (apply merge-bags
            (map skolemized-top-skolem-constraints children))
     (op (map skolemized-formula children))))
  (define (skolemize-children op formula skolem-depth negated)
    (let ([children
           (map
            (λ (child) (skolemize-body child rep-env skolem-depth non-skolems negated cache))
            (node/formula/op-children formula))])
      (merge op children)))
  (match formula
    [(? node/formula/op/=>?)
     (let* ([args (node/formula/op-children formula)]
            [left (car args)]
            [right (cadr args)]
            [skolem-depth (if negated skolem-depth -1)])
       (merge node/formula/op/=>
              (list (skolemize-body left rep-env skolem-depth non-skolems #f cache)
                    (skolemize-body right rep-env skolem-depth non-skolems negated cache))))]
    [(? node/formula/op/&&?)
     (skolemize-children node/formula/op/&& formula (if negated -1 skolem-depth) negated)]
    [(? node/formula/op/||?)
     (skolemize-children node/formula/op/|| formula (if negated skolem-depth -1) negated)]
    [(? node/formula/op/in?)
     (skolemize-children node/formula/op/in formula skolem-depth negated)]
    [(? node/formula/op/=?)
     (skolemize-children node/formula/op/= formula skolem-depth negated)]
    [(? node/formula/op/!?)
     (skolemize-children node/formula/op/! formula skolem-depth (not negated))]))

(define (skolemize-quantifier quantifier decls f rep-env skolem-depth non-skolems negated cache)
  (define (skolemize)
    ; create skolem relation
    ; calculate bounds on skolem relation
    ; join with nonSkolems to create skolem expression
    ; add skolem expression to repEnv
    ; return composition of range constraints and skolemized formula
    ; add domain constraints to top-skolem-constraints
    (let ([skolems
           (map (curryr create-skolem non-skolems) decls)])
      (let* ([rep-env
              (for/fold
               ([rep-env rep-env])
               ([decl (map car decls)]
                [skolem skolems])
                (cons (cons decl (skolem-expr skolem)) rep-env))]
             [formula-skolemized
              (skolemize-body f rep-env skolem-depth non-skolems negated cache)])
        (skolemized
         (for/fold
          ([new-bounds (skolemized-bounds formula-skolemized)])
          ([skolem skolems])
           (cons (cons
                  (skolem-relation skolem)
                  (skolem-upper-bound skolem))
                 new-bounds))
         (for/fold
          ([new-constraints (skolemized-top-skolem-constraints formula-skolemized)])
          ([domain-constraint (map skolem-domain-constraint skolems)])
           (cons domain-constraint new-constraints))
         ((if (equal? 'all quantifier) => &&)
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
            ([formula-skolemized (skolemize-body f rep-env -1 non-skolems negated cache)])
          (struct-copy
           skolemized
           formula-skolemized
           [formula (node/formula/quantified quantifier decls (skolemized-formula formula-skolemized))]))
        (let*
            ([non-skolems (append decls non-skolems)]
             [formula-skolemized (skolemize-body f rep-env (sub1 skolem-depth) non-skolems negated cache)])
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
    (let* ([skolem-name (format "~a*" (relation-name decl-var))]
           [relation (declare-relation (+ (relation-arity decl-var) (length non-skolems)) skolem-name)]
           [expr (if (> (length non-skolems) 0)
                     (foldl (lambda (non-skolem expr)
                          (join (car non-skolem) expr)) relation non-skolems)
                     relation)]
           [upper-bound
            ; TODO: append decl-expr?
            (if (> (length non-skolems) 0)
             (-> (if (> (length non-skolems) 1)
                    (apply -> (map cdr non-skolems))
                    (cdar non-skolems))
                decl-expr)
             decl-expr)]
           [domain-constraint
            (if (> (length non-skolems) 0)
                (in
                 (apply join relation (make-list (relation-arity decl-var) univ))
                 (comprehension non-skolems true))
                true)]
           [range-constraints (&& (in expr decl-expr) (one expr))])
      (skolem relation expr upper-bound domain-constraint range-constraints))))

(define (skolemize f skolem-depth cache?)
  (skolemize-body f empty skolem-depth empty #f
                  (if cache? (make-hash) #f)))