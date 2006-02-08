;; Copyright 2006 Hannes Mehnert, Florian Lorenzen, Fabian Otto
;;
;; This file is part of gennf.
;;
;; gennf is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; gennf is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gennf; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; $Id: F-48D1C070D93800E7560AEE00EF78D0B2.lisp,v 1.10 2006/02/08 19:59:49 florenz Exp $

;; This file contains various functions and macros that
;; do not fit into any of the other files.
;; If it grows larger, it will be possible to split it in
;; a sensible fashion.

(in-package :gennf)

(defmacro extract (symbol symbol-alist)
  "Lookup a an entry in a symbol alist (a symbol
alist is an alist having symbols as keys)."
  `(cdr (assoc ,symbol ,symbol-alist)))

(defun unassoc (symbol symbol-alist)
  "Remove all entries having symbol from a symbol alist."
  (remove-if (lambda (pair) (eql symbol (car pair))) symbol-alist))

(defun reassoc (symbol data symbol-alist)
  "Exchange the value associated with symbol by data in an alist."
  (acons symbol data (unassoc symbol symbol-alist)))

(defmacro with-gensyms ((&rest names) &body forms)
  "Generate symbols for all names given to be used in
a macro. Taken from Peter Seibel's book, chapter 8."
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@forms))

(defmacro ensure-string-pathname (pathspec)
  "SETF pathspec to its namestring if it is
a pathname.
FIXME: This should go into port-path."
  `(when (typep ,pathspec 'pathname)
    (setf ,pathspec (namestring ,pathspec))))

(defun search-multiple (sequences sequence)
  "Search all items of sequences in sequences
and return those that are found."
  (let ((results ()))
    (dolist (item sequences)
      (when (search item sequence)
	(push item results)))
    results))

(defmacro retry-while-condition ((condition variable &rest cleanup-forms)
				 &body body)
  "Executes body as long as condition is repetitiously
signalled. When condition is signalles, cleanup-forms
are executed with variable bound to condition object."
  (with-gensyms (retry pass-through-condition)
    `(loop with ,retry do
      (setf ,retry nil)
      (handler-case (progn ,@body)
	(,condition (,pass-through-condition)	
		    (let ((,variable ,pass-through-condition))
		      ,@cleanup-forms)
		    (setf ,retry t)))
      while ,retry)))


(defmacro retry (retry-specifications &body forms)
  "This macro is a generalization of retry-while-condition.
It takes several conditions and clean-up forms. If one
of those conditions is signalled its cleanup form is
executed and the macro's body repeated.
It also takes a maximum repetition argument per
condition.
The form of one retry-specification is

(condition &key condition-variable maximum cleanup)

condition-variable is available for cleanup. cleanup
must be a single form, progn may be useful.
If maximum is omitted the corresponding condition will
always cause cleanup and repitition.

An example looks like this:

;; (retry (('some-error :maximum 5 :condition-variable error
;;                      :cleanup (progn (inspect error) (repair-the-stuff)))
;;         ('another-exception :cleanup (failure-recover)))
;;   (do-some-work)
;;   (do-some-more-work))

Macroexpansion yields:

;; (LET ((#:G1844 (LIST (0 0))))
;;  (LOOP WITH
;;        #:G1843
;;        DO
;;        (SETF #:G1843 NIL)
;;        (HANDLER-CASE (PROGN (DO-SOME-WORK) (DO-SOME-MORE-WORK))
;;                      ('ANOTHER-EXCEPTION NIL (FAILURE-RECOVER)
;;                       (SET #:G1843 T))
;;                      ('SOME-ERROR (ERROR)
;;                       (PROGN (INSPECT ERROR) (REPAIR-THE-STUFF))
;;                       (WHEN (< (NTH 1 #:G1844) 5)
;;                         (SETF #:G1843 T)
;;                         (INCF (NTH 1 #:G1844)))))
;;        WHILE
;;        #:G1843))

Another hint: If a variable, which is used in the macro's
body, is to be used in cleanup, too, there is no other choice as
to either make is special or to wrap the whole macro with a let
to introduce the variable lexically:

;; (let ((some-variable some-initialization))
;;   (retry ((exception :cleanup (some-form-using-some-variable)))
;;     (some-code-also-using-some-variable)))

FIXME: * If one of the maximums is exceeded a condition should be thrown.
       * This macro is not yet fully tested."
  (with-gensyms (retry counters)
    (let ((conditions nil)
	  (condition-variables nil)
	  (maxima nil)
	  (cleanups nil))
      (mapcar
       #'(lambda (specification)
	   (destructuring-bind
		 (condition
		  &key condition-variable maximum cleanup)
	       specification
	     (push condition conditions)
	     (push condition-variable condition-variables)
	     (push maximum maxima)
	     (push cleanup cleanups)))
       retry-specifications)
      (let ((loop-code
	     `(loop with ,retry do
	       (setf ,retry nil)
	       (handler-case (progn ,@forms)
		 ,@(mapcar (lambda (condition-number)
			     `(,(nth condition-number conditions)
			       ,(if (nth condition-number condition-variables)
				    `(,(nth condition-number
					    condition-variables))
				    `())
			       ,(nth condition-number cleanups)
			       ,(cond ((nth condition-number maxima)
				       `(when (< (nth ,condition-number
						  ,counters)
					       ,(nth condition-number maxima))
					 (setf ,retry t)
					 (incf (nth ,condition-number
						,counters))))
				      (t `(set ,retry t)))))
			   (loop for i from 0 to (1- (length conditions))
				 collect i)))
	       while ,retry)))
	(if (eval (cons 'or maxima))
	    `(let ((,counters
		    (list ,@(make-list
			     (length conditions) :initial-element 0))))
	      ,loop-code)
	    loop-code)))))