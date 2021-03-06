#lang rosette
(require colocolo colocolo/lang/ast colocolo/engine/interpretation colocolo/lang/bounds colocolo/engine/sat/solver colocolo/lib/skolemize-solve colocolo/engine/symmetry)
(define universe$0 (universe (list "0" "1" "2" "3" "4" "5" "6")))
(define v$1 (declare-relation 1 "y"))
(define v$5 (declare-relation 1 "b"))
(define r$2 (declare-relation 3 "square"))
(define v$3 (declare-relation 1 "z"))
(define v$0 (declare-relation 1 "x"))
(define v$6 (declare-relation 1 "x"))
(define v$4 (declare-relation 1 "a"))
(define b-ex$20 (join v$5 r$2))
(define decl$7 (cons v$3 univ #|one|#))
(define decl$17 (cons v$4 univ #|one|#))
(define decl$18 (cons v$5 univ #|one|#))
(define decl$0 (cons v$0 univ #|one|#))
(define b-ex$31 (-> v$6 v$6))
(define b-ex$8 (join v$3 r$2))
(define decl$30 (cons v$6 univ #|one|#))
(define decl$1 (cons v$1 univ #|one|#))
(define b-ex$3 (join v$0 r$2))
(define b-ex$11 (join univ r$2))
(define b-ex$21 (join v$4 b-ex$20))
(define b-ex$12 (join v$3 b-ex$11))
(define b-ex$32 (-> b-ex$31 v$6))
(define decls$2 (list decl$0 decl$1))
(define b-ex$4 (join v$1 b-ex$3))
(define b-ex$9 (join univ b-ex$8))
(define decls$19 (list decl$17 decl$18))
(define b-ex$22 (join b-ex$21 r$2))
(define cmp-f$33 (in b-ex$32 r$2))
(define cmp-f$13 (in univ b-ex$12))
(define cmp-f$10 (in univ b-ex$9))
(define mul-f$5 (multiplicity-formula 'one b-ex$4))
(define q-f$6 (quantified-formula 'all decls$2 mul-f$5))
(define q-f$34 (quantified-formula 'all (list decl$30) cmp-f$33))
(define b-ex$23 (join v$5 b-ex$22))
(define b-f$14 (&& cmp-f$10 cmp-f$13))
(define q-f$15 (quantified-formula 'all (list decl$7) b-f$14))
(define b-ex$24 (join b-ex$23 r$2))
(define b-ex$25 (join v$5 b-ex$24))
(define b-f$16 (&& q-f$6 q-f$15))
(define b-ex$26 (& b-ex$25 v$4))
(define mul-f$27 (multiplicity-formula 'some b-ex$26))
(define q-f$28 (quantified-formula 'all decls$19 mul-f$27))
(define b-f$29 (&& b-f$16 q-f$28))
(define b-f$35 (&& b-f$29 q-f$34))
(define ts$1 (list))
(define ts$2 (list (list "0" "0" "0") (list "0" "0" "1") (list "0" "0" "2") (list "0" "0" "3") (list "0" "0" "4") (list "0" "0" "5") (list "0" "0" "6") (list "0" "1" "0") (list "0" "1" "1") (list "0" "1" "2") (list "0" "1" "3") (list "0" "1" "4") (list "0" "1" "5") (list "0" "1" "6") (list "0" "2" "0") (list "0" "2" "1") (list "0" "2" "2") (list "0" "2" "3") (list "0" "2" "4") (list "0" "2" "5") (list "0" "2" "6") (list "0" "3" "0") (list "0" "3" "1") (list "0" "3" "2") (list "0" "3" "3") (list "0" "3" "4") (list "0" "3" "5") (list "0" "3" "6") (list "0" "4" "0") (list "0" "4" "1") (list "0" "4" "2") (list "0" "4" "3") (list "0" "4" "4") (list "0" "4" "5") (list "0" "4" "6") (list "0" "5" "0") (list "0" "5" "1") (list "0" "5" "2") (list "0" "5" "3") (list "0" "5" "4") (list "0" "5" "5") (list "0" "5" "6") (list "0" "6" "0") (list "0" "6" "1") (list "0" "6" "2") (list "0" "6" "3") (list "0" "6" "4") (list "0" "6" "5") (list "0" "6" "6") (list "1" "0" "0") (list "1" "0" "1") (list "1" "0" "2") (list "1" "0" "3") (list "1" "0" "4") (list "1" "0" "5") (list "1" "0" "6") (list "1" "1" "0") (list "1" "1" "1") (list "1" "1" "2") (list "1" "1" "3") (list "1" "1" "4") (list "1" "1" "5") (list "1" "1" "6") (list "1" "2" "0") (list "1" "2" "1") (list "1" "2" "2") (list "1" "2" "3") (list "1" "2" "4") (list "1" "2" "5") (list "1" "2" "6") (list "1" "3" "0") (list "1" "3" "1") (list "1" "3" "2") (list "1" "3" "3") (list "1" "3" "4") (list "1" "3" "5") (list "1" "3" "6") (list "1" "4" "0") (list "1" "4" "1") (list "1" "4" "2") (list "1" "4" "3") (list "1" "4" "4") (list "1" "4" "5") (list "1" "4" "6") (list "1" "5" "0") (list "1" "5" "1") (list "1" "5" "2") (list "1" "5" "3") (list "1" "5" "4") (list "1" "5" "5") (list "1" "5" "6") (list "1" "6" "0") (list "1" "6" "1") (list "1" "6" "2") (list "1" "6" "3") (list "1" "6" "4") (list "1" "6" "5") (list "1" "6" "6") (list "2" "0" "0") (list "2" "0" "1") (list "2" "0" "2") (list "2" "0" "3") (list "2" "0" "4") (list "2" "0" "5") (list "2" "0" "6") (list "2" "1" "0") (list "2" "1" "1") (list "2" "1" "2") (list "2" "1" "3") (list "2" "1" "4") (list "2" "1" "5") (list "2" "1" "6") (list "2" "2" "0") (list "2" "2" "1") (list "2" "2" "2") (list "2" "2" "3") (list "2" "2" "4") (list "2" "2" "5") (list "2" "2" "6") (list "2" "3" "0") (list "2" "3" "1") (list "2" "3" "2") (list "2" "3" "3") (list "2" "3" "4") (list "2" "3" "5") (list "2" "3" "6") (list "2" "4" "0") (list "2" "4" "1") (list "2" "4" "2") (list "2" "4" "3") (list "2" "4" "4") (list "2" "4" "5") (list "2" "4" "6") (list "2" "5" "0") (list "2" "5" "1") (list "2" "5" "2") (list "2" "5" "3") (list "2" "5" "4") (list "2" "5" "5") (list "2" "5" "6") (list "2" "6" "0") (list "2" "6" "1") (list "2" "6" "2") (list "2" "6" "3") (list "2" "6" "4") (list "2" "6" "5") (list "2" "6" "6") (list "3" "0" "0") (list "3" "0" "1") (list "3" "0" "2") (list "3" "0" "3") (list "3" "0" "4") (list "3" "0" "5") (list "3" "0" "6") (list "3" "1" "0") (list "3" "1" "1") (list "3" "1" "2") (list "3" "1" "3") (list "3" "1" "4") (list "3" "1" "5") (list "3" "1" "6") (list "3" "2" "0") (list "3" "2" "1") (list "3" "2" "2") (list "3" "2" "3") (list "3" "2" "4") (list "3" "2" "5") (list "3" "2" "6") (list "3" "3" "0") (list "3" "3" "1") (list "3" "3" "2") (list "3" "3" "3") (list "3" "3" "4") (list "3" "3" "5") (list "3" "3" "6") (list "3" "4" "0") (list "3" "4" "1") (list "3" "4" "2") (list "3" "4" "3") (list "3" "4" "4") (list "3" "4" "5") (list "3" "4" "6") (list "3" "5" "0") (list "3" "5" "1") (list "3" "5" "2") (list "3" "5" "3") (list "3" "5" "4") (list "3" "5" "5") (list "3" "5" "6") (list "3" "6" "0") (list "3" "6" "1") (list "3" "6" "2") (list "3" "6" "3") (list "3" "6" "4") (list "3" "6" "5") (list "3" "6" "6") (list "4" "0" "0") (list "4" "0" "1") (list "4" "0" "2") (list "4" "0" "3") (list "4" "0" "4") (list "4" "0" "5") (list "4" "0" "6") (list "4" "1" "0") (list "4" "1" "1") (list "4" "1" "2") (list "4" "1" "3") (list "4" "1" "4") (list "4" "1" "5") (list "4" "1" "6") (list "4" "2" "0") (list "4" "2" "1") (list "4" "2" "2") (list "4" "2" "3") (list "4" "2" "4") (list "4" "2" "5") (list "4" "2" "6") (list "4" "3" "0") (list "4" "3" "1") (list "4" "3" "2") (list "4" "3" "3") (list "4" "3" "4") (list "4" "3" "5") (list "4" "3" "6") (list "4" "4" "0") (list "4" "4" "1") (list "4" "4" "2") (list "4" "4" "3") (list "4" "4" "4") (list "4" "4" "5") (list "4" "4" "6") (list "4" "5" "0") (list "4" "5" "1") (list "4" "5" "2") (list "4" "5" "3") (list "4" "5" "4") (list "4" "5" "5") (list "4" "5" "6") (list "4" "6" "0") (list "4" "6" "1") (list "4" "6" "2") (list "4" "6" "3") (list "4" "6" "4") (list "4" "6" "5") (list "4" "6" "6") (list "5" "0" "0") (list "5" "0" "1") (list "5" "0" "2") (list "5" "0" "3") (list "5" "0" "4") (list "5" "0" "5") (list "5" "0" "6") (list "5" "1" "0") (list "5" "1" "1") (list "5" "1" "2") (list "5" "1" "3") (list "5" "1" "4") (list "5" "1" "5") (list "5" "1" "6") (list "5" "2" "0") (list "5" "2" "1") (list "5" "2" "2") (list "5" "2" "3") (list "5" "2" "4") (list "5" "2" "5") (list "5" "2" "6") (list "5" "3" "0") (list "5" "3" "1") (list "5" "3" "2") (list "5" "3" "3") (list "5" "3" "4") (list "5" "3" "5") (list "5" "3" "6") (list "5" "4" "0") (list "5" "4" "1") (list "5" "4" "2") (list "5" "4" "3") (list "5" "4" "4") (list "5" "4" "5") (list "5" "4" "6") (list "5" "5" "0") (list "5" "5" "1") (list "5" "5" "2") (list "5" "5" "3") (list "5" "5" "4") (list "5" "5" "5") (list "5" "5" "6") (list "5" "6" "0") (list "5" "6" "1") (list "5" "6" "2") (list "5" "6" "3") (list "5" "6" "4") (list "5" "6" "5") (list "5" "6" "6") (list "6" "0" "0") (list "6" "0" "1") (list "6" "0" "2") (list "6" "0" "3") (list "6" "0" "4") (list "6" "0" "5") (list "6" "0" "6") (list "6" "1" "0") (list "6" "1" "1") (list "6" "1" "2") (list "6" "1" "3") (list "6" "1" "4") (list "6" "1" "5") (list "6" "1" "6") (list "6" "2" "0") (list "6" "2" "1") (list "6" "2" "2") (list "6" "2" "3") (list "6" "2" "4") (list "6" "2" "5") (list "6" "2" "6") (list "6" "3" "0") (list "6" "3" "1") (list "6" "3" "2") (list "6" "3" "3") (list "6" "3" "4") (list "6" "3" "5") (list "6" "3" "6") (list "6" "4" "0") (list "6" "4" "1") (list "6" "4" "2") (list "6" "4" "3") (list "6" "4" "4") (list "6" "4" "5") (list "6" "4" "6") (list "6" "5" "0") (list "6" "5" "1") (list "6" "5" "2") (list "6" "5" "3") (list "6" "5" "4") (list "6" "5" "5") (list "6" "5" "6") (list "6" "6" "0") (list "6" "6" "1") (list "6" "6" "2") (list "6" "6" "3") (list "6" "6" "4") (list "6" "6" "5") (list "6" "6" "6")))
(define bd$3 (bound r$2 ts$1 ts$2))
(define bounds$4 (bounds universe$0 (list bd$3)))
(displayln "-- skolemizing...")
(match-define (cons F bnds) (time (skolemize-merge bounds$4 b-f$35 3)))
(displayln "-- instantiating bounds...")
(define interp (time (instantiate-bounds bnds)))
(displayln "-- breaking symmetry...")
(define sbp (time (generate-sbp interp bnds)))
(displayln "-- making boolean interpretation...")
(define F* (time (interpret* F interp)))
(define SS (make-SAT))
(displayln "-- making optimized SAT call...")
(define sol (time (SAT-solve SS (list (&& F* sbp)))))
