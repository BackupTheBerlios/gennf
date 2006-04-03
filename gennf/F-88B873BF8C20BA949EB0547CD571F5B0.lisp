;; Copyright 2006 Florian Lorenzen, Fabian Otto
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
;; $Id: F-88B873BF8C20BA949EB0547CD571F5B0.lisp,v 1.9 2006/04/03 17:28:29 florenz Exp $

;; All functions that call SBCL specific extensions (i. e. funtions
;; of some sb-* package) go in here.
;; To support other Common Lisp implementations a similair
;; file for that implementation has to be provided.

(in-package :gennf)


(defun invoke-program (program arguments)
  "Invokes program connecting the in- and output channels to the
standard channels."
  (sb-ext:process-exit-code
   (sb-ext:run-program program arguments
		       :search t
		       :error *error-output*
		       :input t
		       :output t)))

(defun invoke-program-silently (program arguments)
  "Invokes program discarding all its output. This policy is overriden
when in debug mode. In the latter case all the programs output to standard
error resp. standard output is written to the invoker's output."
  (sb-ext:process-exit-code
   (sb-ext:run-program program arguments
		       :search t
		       :error (if *debug-mode* *error-output* nil)
		       :input nil
		       :output (if *debug-mode* *error-output* nil))))

(defun posix-arguments ()
  "Returns the command line arguments."
  sb-ext:*posix-argv*)

(defun quit (&optional (exit-value 0))
  "Quits the Lisp process."
  (sb-ext:quit :unix-status exit-value))

(defmacro with-program-output ((program arguments &key exit-code
					output error) &body forms)
  "Macro to invoke a program and connect its output channels to
given streams. All streams that are not conncted to are discarded."
  (with-gensyms (process)
    `(let ((,process (sb-ext:run-program ,program
					 ,arguments
					 :search t
					 :output :stream
					 :error :stream)))
      (let ,(remove ()
		    (list (when exit-code
			    `(,exit-code (sb-ext:process-exit-code ,process)))
			  (when output
			    `(,output (sb-ext:process-output ,process)))
			  (when error
			    `(,error (sb-ext:process-error ,process)))))
	,@forms
	,@(remove () (list (when output `(close ,output))
			   (when error `(close ,error))))))))
