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
;; $Id: F-88B873BF8C20BA949EB0547CD571F5B0.lisp,v 1.3 2006/02/07 18:05:08 florenz Exp $

;; All functions that call SBCL specific extensions (i. e. funtions
;; of some sb-* package) go in here.
;; To support other Common Lisp implementations a similair
;; file has to be provided.

(in-package :gennf)

(defun invoke-program (program arguments)
  (sb-ext:process-exit-code
   (sb-ext:run-program program arguments
		       :search t
		       :error *error-output*
		       :input t
		       :output t)))

(defmacro with-program-output ((program arguments &key exit-code
					output error) &body forms)
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