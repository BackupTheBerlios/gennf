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
;; $Id: F-4E51556B59366B0B171CCB0B1F4F10A9.lisp,v 1.3 2006/01/18 18:22:54 florenz Exp $

(in-package :gennf)

(defparameter *cvs-import-log-message* *backend-import-log-message*)
(defparameter *cvs-import-vendor-tag* "gennf-created")
(defparameter *cvs-import-release-tag* "fresh-repository")
(defparameter *cvs-command-name* "cvs")

(defmacro with-cvs-output ((arguments &key exit-code
				      output error) &body forms)
  `(with-program-output (*cvs-command-name*
			 ,arguments
			 :output ,output
			 :error ,error
			 :exit-code ,exit-code)
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

(defun cvs-commit (message access files)
  (let (file-list argument-list)
    (dolist (file files)
      (unless (cvs-known-file-p access file)
	(cvs-add access (list file)))
      (push file file-list))
    (setf argument-list (append (list "-d" (extract :root access)
				      "ci" "-m" message)
				file-list))
    (with-cvs-output (argument-list :error error :exit-code exit-code)
      (unless (= exit-code 0)
	(let ((outdated-files
	       (loop for line = (read-line error nil)
		     while line
		     when (search "Up-to-date check failed" line)
		     append (search-multiple file-list line))))
	  (setf outdated-files (mapcar #'pathname outdated-files))
	  (error 'backend-outdated-error :code exit-code
		 :description "Some files are outdated."
		 :files outdated-files))))))

(defun cvs-up-to-date-p (access file)
  (ensure-string-pathname file)
  (with-cvs-output ((list "-d" (extract :root access)
			  "-n" "up" file)
		    :output stream)
    (let ((output (read-line stream nil)))
      (not (and output 
		(char/= (aref output 0) #\A)
		(char/= (aref output 0) #\?))))))
	  
(defun cvs-known-file-p (access file)
  (ensure-string-pathname file)
  (with-cvs-output ((list "-d" (extract :root access)
			     "log" file)
		       :exit-code exit-code)
    (= exit-code 0)))

(defun cvs-add (access files)
  "Do a cvs add for all files. Files may be have a
directory part. In this case, all directories are added
to the repository."
  (dolist (file files)
    (let ((directory-prefixes (cdr (pathname-prefixes file)))
	  (paths (append directory-prefixes (list file))))
      (dolist (path paths)
	(ensure-string-pathname path)
	(invoke-cvs "-d" (extract :root access)
		    "add"
		    path)))))
    