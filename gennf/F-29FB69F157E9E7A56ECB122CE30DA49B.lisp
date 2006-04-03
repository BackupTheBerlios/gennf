;; packages.lisp
;;
;; Part of fd, written by Florian Lorenzen <florenz@cs.tu-berlin.de>
;;
;; Package definitions for fd.
;; 

(in-package :cl-user)

(defpackage fd
  (:use :cl)
  (:export
   :interval
   :alldifferent
   :node-consistency
   :arc-consistency
   :local-consistency
   :backtrack-solv
   :dom-red-solv
   :fd-solv
   :fd-entail
   :domains-equal
   :show-solution
   :solutions-equal
   :fd-delay
   :fd-add :fd-sub :fd-mul :fd-div :fd-mod :fd-pow
   :fd-eq :fd-neq :fd-lt :fd-gt :fd-le :fd-ge :fd-in)
  (:documentation "Simple finite domain constraint solver."))

(defpackage sudoku
  (:use :cl :fd)
  (:export
   :solve-sudoku
   :*easy*
   :*insane*)
  (:documentation "Solving Sodoku puzzles with fd."))

(defpackage send-more-money
  (:use :cl :fd)
  (:export
   :*send-more-money*
   :*end-ore-oney*)
  (:documentation "Solve the send+more=money problem with fd."))

(defpackage fd-test
  (:use :cl :fd :sudoku :com.gigamonkeys.test)
  (:export :test-short-fd :test-long-fd)
  (:documentation "Test cases for the fd solver."))
