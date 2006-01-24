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
;; $Id: F-4E51556B59366B0B171CCB0B1F4F10A9.lisp,v 1.18 2006/01/24 20:17:25 florenz Exp $

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

;; (defmacro cvs-default-error-handling (&rest arguments)
;;   (let ((exit-code (gensym "exit-code-")))
;;     `(let ((,exit-code
;; 	    (invoke-cvs ,@arguments)))
;;       (cvs-default-error-handler ,exit-code))))

  
(defun cvs-default-error-handler (exit-code)
  (cond ((= exit-code 0) exit-code)
	(t (error "cvs had exit code ~S." exit-code))))

(defun invoke-cvs (&rest arguments)
  (invoke-program *cvs-command-name* arguments))

(defun cvs-default-error-handling (&rest args)
  (cvs-default-error-handler (apply #'invoke-cvs args)))


(defun cvs-import (module access)
  (cvs-default-error-handling "-d" (extract :root access)
			      "import"
			      "-m" *cvs-import-log-message*
			      module
			      *cvs-import-vendor-tag*
			      *cvs-import-release-tag*))

(defun cvs-get (module access files destination)
  "For some reason  co -d . ist not possible with cvs using
ssh as transport layer. That means that all files
are checked out into gennf-tmp/module/. The whole content of
gennf-tmp/module is moved to destination,
especially including the cvs meta directory, because
cvs-commit would not work otherwise.
temporary-directory should be generated using some
save routine."
  (in-temporary-directory
    (let* ((file-list (mapcar #'(lambda (file)
				  (format nil "~A/~A" module
					  (namestring file)))
			      files))
	   (argument-list (append (list "-d" (extract :root access) "co")
				  file-list))
	   ;;    (cvs-command (append (list 'cvs-default-error-handling
	   ;; 				      "-d" (extract :root access)
	   ;; 				      "co")
	   ;; 				file-list))
	   (module-path (make-pathname :directory
				       (list :relative module))))
      (apply #'cvs-default-error-handling argument-list)
      ;;       (eval cvs-command)		
      (port-path:with-directory-form ((destination-directory
				       destination))
	(move-directory-tree module-path
			     destination-directory)
	(delete-directory-tree module-path)))))

(defun cvs-commit (message access files)
  "If files are not known to cvs they are added.
The current working directory has to be a result of a cvs-get.
If some files are outdated a condition backend-outdated-error
is signalled, containing a list of outdated files. In this
case no files are committed at all."
  (let (file-list argument-list)
    (dolist (file files)
      (unless (cvs-known-file-p access file)
	(cvs-add access (list file)))
      (push file file-list))
    ;(ensure-string-pathname message-file)
    (setf argument-list (append (list "-d" (extract :root access)
				      "ci" "-m" message)
				(mapcar #'namestring file-list)))
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
  "Do a cvs add for all files. files have to be in
file-form, no directories! Files may be have a
directory part. In this case, all directories are added
to the repository."
  (dolist (file files)
    (let* ((directory-prefixes (cdr (pathname-prefixes file)))
	   (paths (append directory-prefixes (list file))))
      (dolist (path paths)
	(ensure-string-pathname path)
	(invoke-cvs "-d" (extract :root access)
		    "add"
		    path)))))
