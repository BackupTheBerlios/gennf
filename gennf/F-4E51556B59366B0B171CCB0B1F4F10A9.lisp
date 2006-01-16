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
;; $Id: F-4E51556B59366B0B171CCB0B1F4F10A9.lisp,v 1.1 2006/01/16 07:47:42 florenz Exp $

(in-package :gennf)

(defparameter *cvs-import-log-message* *backend-import-log-message*)
(defparameter *cvs-import-vendor-tag* "gennf-created")
(defparameter *cvs-import-release-tag* "fresh-repository")
(defparameter *cvs-command-name* "cvs")

(defmacro with-cvs-output ((output arguments) &body forms)
  `(with-program-output (*cvs-command-name*
			 ,arguments
			 :output ,output)
    ,@forms))

(defmacro with-cvs-exit-code ((exit-code arguments) &body forms)
  `(with-program-output (*cvs-command-name*
			 ,arguments
			 :exit-code ,exit-code)
    ,@forms))

(defmacro with-cvs-exit-code-and-output ((exit-code output arguments)
						    &body forms)
  `(with-program-output (*cvs-command-name*
			 ,arguments
			 :exit-code ,exit-code
			 :output ,output)
    ,@forms))

(defmacro cvs-default-error-handling (&rest arguments)
  (let ((exit-code (gensym "exit-code-")))
    `(let ((,exit-code
	    (invoke-cvs ,@arguments)))
      (cvs-default-error-handler ,exit-code))))

(defun cvs-default-error-handler (exit-code)
  (cond ((= exit-code 0) exit-code)
	(t (error "cvs had exit code ~S." exit-code))))

(defun invoke-cvs (&rest arguments)
  (invoke-program *cvs-command-name* arguments))

(defun cvs-import (module access)
  (cvs-default-error-handling "-d" (extract :root access)
			      "import"
			      "-m" *cvs-import-log-message*
			      module
			      *cvs-import-vendor-tag*
			      *cvs-import-release-tag*))

(defun cvs-get (module access files)
  (dolist (file files)
    (ensure-string-pathname file)
    (let ((path (format nil "~A/~A" module file)))
      (cvs-default-error-handling "-d" (extract :root access)
				  "co"
				  "-d" "."
				  path))))

(defun cvs-commit ()
  ())

;(defun cvs-commit (message access files)
;  (let (file-list argument-list)
;    (dolist (file files)
;      (ensure-string-pathname file)
;      (unless (cvs-known-file-p access file)
;	(cvs-add access file))
;      (push file file-list))
;    (setf argument-list (append (list "-d" (extract :root access) "ci")
;				file-list))
;    (with-cvs-exit-code-and-output (exit-code output argument-list)
;      (unless (= exit-code 0)
;	(let ((outdated-files ()))
;	  (loop for line = (read-line output nil)
;		while line do
;		(when (search "Up-to-date check failed" line)
;		  (setf outdated-files (concatenate
;					(search-multiple file-list line)
;					outdated-files))
		  
		    
		 
				 

;  (let ((outdated-files ()))
;    (dolist (file files)
 
;      (unless (up-to-date-p access file)
;	(push file outdated-files)))
;    (when outdated-files
;      (error 'backend-outdated-error :files outdated-files))


(defun cvs-up-to-date-p (access file)
  (ensure-string-pathname file)
  (with-cvs-output (stream (list "-d" (extract :root access)
				 "-n"
				 "up"
				 file))
    (let ((output (read-line stream nil)))
      (not (and output 
		(char/= (aref output 0) #\A)
		(char/= (aref output 0) #\?))))))
	  
(defun cvs-known-file-p (access file)
  (ensure-string-pathname file)
  (with-cvs-exit-code (exit-code (list "-d" (extract :root access)
				       "log"
				       "file"))
    (= exit-code 0)))

(defun cvs-add (access files)
  (dolist (file files)
    (ensure-string-pathname file)
    (invoke-cvs "-d" (extract :root access)
		"add"
		file)))