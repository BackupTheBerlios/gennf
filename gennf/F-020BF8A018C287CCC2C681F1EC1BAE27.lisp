;; tests.lisp
;;
;; Part of fd, written by Florian Lorenzen <florenz@cs.tu-berlin.de>
;;
;; Small testsuite for fd.
;;


(in-package :fd-test)


;;
;; Small example CSPs.
;;

(defparameter *csp1*
  '((fd-in x (3 5 6 7 9)) (fd-in y (2 3 4 5 6))
    (fd-neq x 7) (fd-le (fd-add x y) 8))
  "Has a solution, [PH04], p. 50.")

(defparameter *csp2*
  '((fd-in x (0 1)) (fd-in y (0 1)) (fd-in z (0 1))
    (fd-neq x y) (fd-neq x z) (fd-neq y z))
  "Is inconsistent, [PH04], p. 54.")

(defparameter *csp3*
  '((fd-in x (1 3 5 7)) (fd-in y (1 2 3 4 5 6 7))
    (fd-le x 3) (fd-ge y 5) (fd-lt (fd-mul x y) 7)))

(defparameter *csp4*
  '((fd-in x (1 2 3)) (fd-in y (2 4 6)) (fd-in z (1 3))
    (fd-eq (fd-add x (fd-mul y z)) 8)))


;;
;; Tests for the basic functions.
;;

(deftest test-node-consistency ()
  (check
    (domains-equal (node-consistency *csp1*)
		   '((fd-in x (3 5 6 9)) (fd-in y (2 3 4 5 6))))
    (domains-equal (node-consistency *csp2*) *csp2*)
    (domains-equal (node-consistency *csp3*)
		   '((fd-in x (1 3)) (fd-in y (5 6 7))))
    (domains-equal (node-consistency *csp4*) *csp4*)))

(deftest test-arc-consistency ()
  (check
    (domains-equal (arc-consistency (node-consistency *csp1*))
		   '((fd-in x (3 5 6)) (fd-in y (2 3 4 5))))
    (domains-equal (arc-consistency (node-consistency *csp2*)) *csp2*)
    (domains-equal (arc-consistency (node-consistency *csp3*))

		   '((fd-in x (1)) (fd-in y (5 6))))
    (domains-equal (arc-consistency (node-consistency *csp4*)) *csp4*)))

(deftest test-local-consistency ()
  (check
    (domains-equal (local-consistency (node-consistency *csp1*))
		   '((fd-in x (3 5 6)) (fd-in y (2 3 4 5))))
    (domains-equal (local-consistency (node-consistency *csp2*)) *csp2*)
    (domains-equal (local-consistency (node-consistency *csp3*))
		   '((fd-in x (1)) (fd-in y (5 6))))
    (domains-equal (local-consistency (node-consistency *csp4*))
		   '((fd-in x (2)) (fd-in y (2 6)) (fd-in z (1 3))))))


;;
;; Tests for the solver functions.
;;

(deftest test-backtrack-solv ()
  (check
    (backtrack-solv *csp1*)
    (not (backtrack-solv *csp2*))
    (backtrack-solv *csp3*)
    (backtrack-solv *csp4*)))

(deftest test-dom-red-solv ()
  (check
    (dom-red-solv *csp1*)
    (not (dom-red-solv *csp2*))
    (dom-red-solv *csp3*)
    (dom-red-solv *csp4*)))
 
(deftest test-fd-entail ()
  (check
    (eql (fd-entail *csp1* '(fd-ge (fd-add x y) 5)) t)
    (eql (fd-entail *csp1* '(fd-gt (fd-add x y) 5)) 'fd-delay)
    (eql (fd-entail *csp3* '(fd-neq x 2)) t)
    (eql (fd-entail *csp1* '(fd-eq (fd-sub x y) 5)) nil)
    (eql (fd-entail *csp4* '(fd-eq x (fd-div 12 y))) 'fd-delay)))


;;
;; A short and a long bunch of tests. The long one solveds Sudoku puzzles.
;;

(deftest test-short-fd ()
  (check
    (test-node-consistency)
    (test-arc-consistency)
    (test-local-consistency)
    (test-backtrack-solv)
    (test-dom-red-solv)
    (test-fd-entail)))

(deftest test-long-fd ()
  (check
    (solutions-equal (solve-sudoku *easy*)
		     '((s11 . 5) (s12 . 1) (s13 . 9) (s14 . 6)
		       (s15 . 2) (s16 . 8) (s17 . 7) (s18 . 4)
		       (s19 . 3) (s21 . 8) (s22 . 4) (s23 . 2)
		       (s24 . 3) (s25 . 1) (s26 . 7) (s27 . 9)
		       (s28 . 5) (s29 . 6) (s31 . 3) (s32 . 7)
		       (s33 . 6) (s34 . 4) (s35 . 9) (s36 . 5)
		       (s37 . 2) (s38 . 8) (s39 . 1) (s41 . 4)
		       (s42 . 6) (s43 . 3) (s44 . 8) (s45 . 7)
		       (s46 . 9) (s47 . 1) (s48 . 2) (s49 . 5)
		       (s51 . 2) (s52 . 9) (s53 . 8) (s54 . 1)
		       (s55 . 5) (s56 . 4) (s57 . 6) (s58 . 3)
		       (s59 . 7) (s61 . 1) (s62 . 5) (s63 . 7)
		       (s64 . 2) (s65 . 6) (s66 . 3) (s67 . 8)
		       (s68 . 9) (s69 . 4) (s71 . 7) (s72 . 3)
		       (s73 . 4) (s74 . 9) (s75 . 8) (s76 . 1)
		       (s77 . 5) (s78 . 6) (s79 . 2) (s81 . 9)
		       (s82 . 2) (s83 . 1) (s84 . 5) (s85 . 3)
		       (s86 . 6) (s87 . 4) (s88 . 7) (s89 . 8)
		       (s91 . 6) (s92 . 8) (s93 . 5) (s94 . 7)
		       (s95 . 4) (s96 . 2) (s97 . 3) (s98 . 1)
		       (s99 . 9)))
    (solutions-equal (solve-sudoku *insane*)
		     '((s11 . 8) (s12 . 3) (s13 . 7) (s14 . 6) (s15 . 9)
		       (s16 . 1) (s17 . 5) (s18 . 4) (s19 . 2) (s21 . 9)
		       (s22 . 4) (s23 . 6) (s24 . 2) (s25 . 5) (s26 . 3)
		       (s27 . 8) (s28 . 1) (s29 . 7) (s31 . 1) (s32 . 2)
		       (s33 . 5) (s34 . 7) (s35 . 4) (s36 . 8) (s37 . 9)
		       (s38 . 6) (s39 . 3) (s41 . 5) (s42 . 9) (s43 . 3)
		       (s44 . 8) (s45 . 2) (s46 . 4) (s47 . 1) (s48 . 7)
		       (s49 . 6) (s51 . 2) (s52 . 6) (s53 . 1) (s54 . 9)
		       (s55 . 7) (s56 . 5) (s57 . 4) (s58 . 3) (s59 . 8)
		       (s61 . 7) (s62 . 8) (s63 . 4) (s64 . 3) (s65 . 1)
		       (s66 . 6) (s67 . 2) (s68 . 5) (s69 . 9) (s71 . 3)
		       (s72 . 5) (s73 . 8) (s74 . 4) (s75 . 6) (s76 . 9)
		       (s77 . 7) (s78 . 2) (s79 . 1) (s81 . 6) (s82 . 1)
		       (s83 . 2) (s84 . 5) (s85 . 8) (s86 . 7) (s87 . 3)
		       (s88 . 9) (s89 . 4) (s91 . 4) (s92 . 7) (s93 . 9)
		       (s94 . 1) (s95 . 3) (s96 . 2) (s97 . 6) (s98 . 8)
		       (s99 . 5)))))
