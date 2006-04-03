;; fd.asd
;;
;; Part of fd, written by Florian Lorenzen <florenz@cs.tu-berlin.de>
;;
;; Systems definition for fd. Follows ASDF-Install conventions.
;;

(defpackage :fd-system
    (:use :cl :asdf))

(in-package :fd-system)

(defsystem :fd
  :name "fd"
  :author "Florian Lorenzen <florenz@cs.tu-berlin.de>"
  :version "0.1"
  :description "Simple finite domain constraint solver.
The algorithms are taken from Petra Hofstedt's script of
her 'Introduction to Constraint Programming' lecture."
  :depends-on (:test-framework)
  :components
  ((:file "packages")
   (:file "fd"
	  :depends-on ("packages"))
   (:file "tests/tests"
	  :depends-on ("packages" "fd" "tests/sudoku"))
   (:file "tests/send-more-money"
	  :depends-on ("packages" "fd"))
   (:file "tests/sudoku"
	  :depends-on ("packages" "fd"))))
