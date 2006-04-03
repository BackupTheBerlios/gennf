;; send-more-money.lisp

(in-package :send-more-money)

(defparameter *variables* '(s e n d m o r y)
  "All variables of send more money.")

(defparameter *all-different* (alldifferent *variables*)
  "Alldifferent constraints")

(defparameter *domains*
  (mapcar #'(lambda (variable) (interval variable 0 9)) *variables*)
  "Domains of all variables.")

(defparameter *addition*
  '((fd-eq
     (fd-add
      (fd-mul 1000 s) (fd-mul 100 e) (fd-mul 10 n) d
      (fd-mul 1000 m) (fd-mul 100 o) (fd-mul 10 r) e)
     (fd-add
      (fd-mul 10000 m) (fd-mul 1000 o) (fd-mul 100 n) (fd-mul 10 e) y)))
  "  S E N D
   + M O R E
   ---------
   M O N E Y")

(defparameter *help*
  '((fd-neq m 0) (fd-neq s 0))
  "Additionel help.")

(defparameter *send-more-money*
  (append *domains* *all-different* *addition* *help*)
  "The complete csp.")

(defparameter *end-ore-oney*
  (let ((variables '(e n d o r y)))
    (append (alldifferent variables)
	    (mapcar #'(lambda (variable) (interval variable 0 9)) variables)
	    '((fd-eq
	       (fd-add
		(fd-mul 100 e) (fd-mul 10 n) d
		(fd-mul 100 o) (fd-mul 10 r) e)
	      (fd-add
	       (fd-mul 1000 o) (fd-mul 100 n) (fd-mul 10 e) y)))))
  "The smaller end-ore-oney problem.")
