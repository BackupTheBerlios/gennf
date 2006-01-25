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
;; $Id: F-48D1C070D93800E7560AEE00EF78D0B2.lisp,v 1.6 2006/01/25 17:49:10 florenz Exp $

(in-package :gennf)

(defmacro extract (symbol symbol-alist)
  `(cdr (assoc ,symbol ,symbol-alist)))

(defun unassoc (symbol symbol-alist)
  (remove-if (lambda (pair) (eql symbol (car pair))) symbol-alist))

(defun reassoc (symbol data symbol-alist)
  (acons symbol data (unassoc symbol symbol-alist)))

(defmacro with-gensyms ((&rest names) &body forms)
  "Taken from Peter Seibel's book, chapter 8."
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@forms))

(defmacro ensure-string-pathname (pathspec)
  `(when (typep ,pathspec 'pathname)
    (setf ,pathspec (namestring ,pathspec))))

(defun search-multiple (sequences sequence)
  (let ((results ()))
    (dolist (item sequences)
      (when (search item sequence)
	(push item results)))
    results))

(defun pathname-prefixes (pathspec)
  "Given a pathspec /dir1/dir2/dir3/file a list of
all directory-prefixes is generated:
/, /dir1/, /dir1/dir2/, /dir1/dir2/dir3/."
  (port-path:with-pathname ((pathname pathspec))
    (let ((directory-prefixes
	   ;; Generate alle prefixes in reverse order. The longest
           ;; prefix is first and all directories appear in reverse
	   ;; order, too.
	   (maplist #'identity (reverse (pathname-directory pathname))))
	  pathname-prefixes)
      (dolist (prefix directory-prefixes)
	;; By using push, the shortest prefix is first in list and
	;; by reversing prefix, directories are put in correct order.
	(push (make-pathname :directory (reverse prefix)
			     :name nil :type nil
			     :defaults pathname) pathname-prefixes))
      pathname-prefixes)))

;; Does this macro leak?
;; More conditions could be supported.
(defmacro retry-until-finished ((condition variable &rest cleanup-forms)
				&body body) 
  (let  ((retry (gensym))
	 (pass-through-condition (gensym)))
    `(loop with ,retry do
      (setf ,retry nil)
      (handler-case (progn ,@body)
	(,condition (,condi)	
	  (let ((,variable ,pass-through-condition))
	    ,@cleanup-forms)
	  (setf ,retry t)))
      while ,retry)))

;; This macro is a generalization of retry-until-finished.
;; It takes several conditions and clean-up forms. If one
;; of those conditions is signalled its cleanup form is
;; executed and the macro's body repeated.
;; It also takes a maximum repetition argument per
;; condition.
;; The form of one retry-specification is
;;
;; (condition &key condition-variable maximum cleanup)
;;
;; condition-variable is available for cleanup. cleanup
;; must be a single form, progn may be useful.
;; If maximum is omitted the corresponding condition will
;; always cause cleanup and repitition.
;;
;; An example looks like this:
;;
;; (retry (('some-error :maximum 5 :condition-variable error
;;                      :cleanup (progn (inspect error) (repair-the-stuff)))
;;         ('another-exception :cleanup (failure-recover)))
;;   (do-some-work)
;;   (do-some-more-work)))
;;
;; Macroexpansion yields:
;;
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
(defmacro retry (retry-specifications &body forms)
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
	     (setf conditions (cons condition conditions))
	     (setf condition-variables (cons condition-variable
					     condition-variables))
	     (setf maxima (cons maximum maxima))
	     (setf cleanups (cons cleanup cleanups))))
       retry-specifications)
      `(let ((,counters (list ,(make-list
				(length conditions) :initial-element 0))))
	(loop with ,retry do
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
	      while ,retry)))))