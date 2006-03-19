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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.36 2006/03/19 23:24:26 florenz Exp $

;; Main module. Basic operations of gennf are implemented in this file.

(in-package :gennf)

;; For development purposes only.
(defparameter *devel-root2*
;  "florenz@fiesta.cs.tu-berlin.de:/home/f/florenz/gennf-junk2")
  "/home/florian/gennf-junk2")
(defparameter *devel-access2*
  (make-instance 'access :root *devel-root2*))
(defparameter *devel-root*
;  "florenz@fiesta.cs.tu-berlin.de:/home/f/florenz/gennf-junk")
  "/home/florian/gennf-junk")
(defparameter *devel-access*
  (make-instance 'access :root *devel-root*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(optimize (cl:debug 3))))
;; End of development only section.


;;
;; Dispatch.
;; 

(defun gennf ()
  "Startup function."
  (handler-case
      (let* ((arguments (rest (posix-arguments)))
	     (command (first arguments))
	     (command-arguments (rest arguments)))
	(debug
	  (debug-format "Command: ~a~%Arguments: ~a~%" command
			command-arguments))
	(unless command
	  (error "Specify a subcommand!~%"))
	(dispatch-subcommand command command-arguments)
	(quit))
    (error (condition)
      (progn
	(error-output "~%~A~%~%" condition)
	(quit)))))

(defun dispatch-subcommand (command command-args)
  (apply (extract command *subcommand-list* :test #'string=) command-args))


;;
;; Subcommands.
;; 




(define-subcommand (foo f fo) subcommand-foo
    (module &key (root r) (branch b) change &rest files)
  (format t "this is foo. ~A ~%" root))


(define-subcommand (bar b ba) subcommand-bar
    (&key (root-destination d) (root-origin o) (change c))
;  :in-meta-directory
  "foo bar command"
  (format t "this is bar. args are ~A.~%" (list root-destination
						root-origin
						change)))







		   

(defsubcommand resync subcommand-resync ()
  ;; FIXME: iles are not properly renamed.
  :in-meta-directory
  (mapcar #'(lambda (mapping)
	      (sync mapping (branch-identifier-to-directory *branch*)))
	  (read-map-file *map-file*)))

(defsubcommand add subcommand-add (&rest namestrings)
  :in-meta-directory
  ;; FIXME: multiple adds!
  (port-path:in-directory *sandbox-directory*
    (dolist (namestring namestrings)
      (let* ((id (format nil "~A" (guid-gen)))
	     (path (pathname namestring))
	     (mapping (create-new-mapping :path path :id id)))
	(add-mapping mapping *map-file*)
	(subcommand-resync)))))

(defsubcommand delete subcommand-delete (&rest namestrings)
  :in-meta-directory
  (port-path:in-directory *sandbox-directory*
    (dolist (namestring namestrings)
      (let ((mapping (get-mapping namestring *map-file*)))
	(remove-mapping mapping *map-file*)
	(delete-file (path mapping))))))

(defsubcommand move subcommand-move (namestring1 namestring2)
  ;; namestring1 and namestring2 have to be relative to the sandbox.
  :in-meta-directory
  (let* ((old-file-relative (pathname namestring1))
	 (new-file-relative (pathname namestring2))
	 (old-file-absolute (merge-pathnames old-file-relative
					     *sandbox-directory*))
	 (old-mapping (get-mapping old-file-relative *map-file*))
	 (new-mapping (create-new-mapping :kind :file 
					  :path new-file-relative
					  :id (id old-mapping))))
    (remove-mapping old-mapping *map-file*)
    (add-mapping new-mapping *map-file*)
    (sync new-mapping (branch-identifier-to-directory *branch*))
    (delete-file old-file-absolute)))

(defsubcommand checkout subcommand-checkout (module
					     &key (root r) (branch b)
					     (change c))
  (let ((working-directory
	 (merge-pathnames (make-pathname :directory (list :relative module))))
	(access (make-instance 'access :root root)))
    (ensure-directories-exist working-directory)
    (port-path:in-directory working-directory
      (distribution-checkout module access
			     (if branch (parse-integer branch) 1)
			     (if change (parse-integer change)))
      (subcommand-resync))))

(defsubcommand setup subcommand-setup (module root
				       &key symbolic-name description)
  (let ((access (make-instance 'access :root root)))
    (create-empty-repository module access)
    (create-empty-branch module access :symbolic-name symbolic-name
			 :description description)
    (format t "The new module can be checked out with:~%")
    (format t "$ gennf checkout ~A ~A~%" module root)))

(defsubcommand commit subcommand-commit (&rest files)
  :in-meta-directory
  (let* ((f-files (translate-to-f-files files *map-file*))
	 (changed-files (sandbox-changed-files *module* *access*
					       *branch* f-files)))
    (debug
      (debug-format "Changed files: ~A" changed-files))
    (if changed-files
	(distribution-commit *module* "<empty>" *access* *branch*
			     changed-files)
	(format t "No files changed. Nothing committed.~%"))))

(defsubcommand update subcommand-update (&rest files)
  :in-meta-directory
  (let ((f-files (translate-to-f-files files *map-file*)))
    (distribution-update *module* *access* *branch* f-files)
    (subcommand-resync)))

(defsubcommand branch subcommand-branch (module root1 root2 branch
					 &optional change)
  (let* ((access1 (make-instance 'access :root root1))
	 (access2 (make-instance 'access :root root2)))
    (unless (backend-known-module-p module access1)
      (format t "Creating new module ~A at ~A.~%" module root1)
      (create-empty-repository module access1))
    (let ((identifier (create-empty-branch module access1)))
      (distribution-merge module access1 identifier
			  access2 (parse-integer branch)
			  (if change (parse-integer change)))
      (format t "Created branch number ~A.~%" identifier)
      (format t "The branch can be checked out with:~%")
      (format t "$ gennf checkout ~A ~A ~A~%" module root1 identifier))))

(defsubcommand merge subcommand-merge (module root1 branch1
				       root2 branch2 &optional change)
  (let* ((access1 (make-instance 'access :root root1))
	 (access2 (make-instance 'access :root root2)))
    (distribution-merge module access1 branch1 access2 branch2 change)))

;;(defsubcommand merge-finish subcommand-merge-finish ()
  