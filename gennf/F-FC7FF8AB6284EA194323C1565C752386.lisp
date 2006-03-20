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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.37 2006/03/20 00:29:58 florenz Exp $

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

(define-subcommand resync subcommand-resync ()
  ;; FIXME: files are not properly renamed.
  :in-meta-directory
  "Resynchronizes the map file and the sandbox."
  (mapcar #'(lambda (mapping)
	      (sync mapping (branch-identifier-to-directory *branch*)))
	  (read-map-file *map-file*)))

(define-subcommand add subcommand-add (&rest files)
  ;; FIXME: multiple adds!
  :in-meta-directory
  "Add files to versioned files."
  (port-path:in-directory *sandbox-directory*
    (dolist (namestring files)
      (let* ((id (format nil "~A" (guid-gen)))
	     (path (pathname namestring))
	     (mapping (create-new-mapping :path path :id id)))
	(add-mapping mapping *map-file*)
	(subcommand-resync)))))

(define-subcommand (delete del rm) subcommand-delete (&rest files)
  :in-meta-directory
  "Exclude files from version control. They are also deleted in
the sandbox."
  (port-path:in-directory *sandbox-directory*
    (dolist (namestring files)
      (let ((mapping (get-mapping namestring *map-file*)))
	(remove-mapping mapping *map-file*)
	(delete-file (path mapping))))))

(define-subcommand (move mv) subcommand-move (old new)
  ;; FIXME: old and new have to be relative to the sandbox,
  ;; this should be changed, relative and absolute filenames should
  ;; be handled correctly.
  :in-meta-directory
  "Move a file. Also for renaming."
  (let* ((old-file-relative (pathname old))
	 (new-file-relative (pathname new))
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

(define-subcommand (checkout co) subcommand-checkout
    (module &key (root r) (branch b) (change c))
  "Check out the indicated branch. If no branch is given 1 is taken."
  (let ((working-directory
	 (merge-pathnames (make-pathname :directory (list :relative module))))
	(access (make-instance 'access :root root)))
    (ensure-directories-exist working-directory)
    (port-path:in-directory working-directory
      (distribution-checkout module access
			     (if branch (parse-integer branch) 1)
			     (if change (parse-integer change)))
      (subcommand-resync))))

(define-subcommand setup subcommand-setup
    (module &key (root r) (symbolic-name n) (description d))
  "Setup a new module."
  (let ((access (make-instance 'access :root root)))
    (create-empty-repository module access)
    (create-empty-branch module access :symbolic-name symbolic-name
			 :description description)
    (format t "The new module can be checked out with:~%")
    (format t "$ gennf checkout ~A ~A~%" module root)))

(define-subcommand (commit ci) subcommand-commit (&rest files)
  :in-meta-directory
  "Commit the given files. If no files are given, all changed
files are committed."
  (let* ((f-files (translate-to-f-files files *map-file*))
	 (changed-files (sandbox-changed-files *module* *access*
					       *branch* f-files)))
    (debug
      (debug-format "Changed files: ~A" changed-files))
    (if changed-files
	(distribution-commit *module* "<empty>" *access* *branch*
			     changed-files)
	(format t "No files changed. Nothing committed.~%"))))

(define-subcommand (update up) subcommand-update (&key (change c) &rest files)
  :in-meta-directory
  "Bring files to latest change or to the indicated change."
  (let ((f-files (translate-to-f-files files *map-file*)))
    (distribution-update *module* *access* *branch* f-files change)
    (subcommand-resync)))

(define-subcommand (branch br) subcommand-branch
    (module &key (root-from f) (root-to t) (branch b) (change c))
  (let* ((access1 (make-instance 'access :root root-from))
	 (access2 (make-instance 'access :root root-to)))
    (unless (backend-known-module-p module access1)
      (format t "Creating new module ~A at ~A.~%" module root-from)
      (create-empty-repository module access1))
    (let ((identifier (create-empty-branch module access1)))
      (distribution-merge module access1 identifier
			  access2 (parse-integer branch)
			  (if change (parse-integer change)))
      (format t "Created branch number ~A.~%" identifier)
      (format t "The branch can be checked out with:~%")
      (format t "$ gennf checkout ~A ~A ~A~%" module root-from identifier))))

(define-subcommand (merge mg) subcommand-merge
    (module &key (root-from f) (branch-from b) (root-to t) (branch-to d)
	    (change c))
  "Merge branch-from into branch-to."
  (let* ((access1 (make-instance 'access :root root-to))
	 (access2 (make-instance 'access :root root-from)))
    (distribution-merge module access1 branch-from access2 branch-to change)))

;;(defsubcommand merge-finish subcommand-merge-finish ()
