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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.10 2006/02/08 19:59:49 florenz Exp $

;; Main module. Basic operations of gennf are implemented in this file.

(in-package :gennf)

;; For development purposes only.
(defparameter *devel-root*
  "florenz@fiesta.cs.tu-berlin.de:/home/f/florenz/gennf-junk")
(defparameter *devel-access*
  (create-new-access :identifier 0 :root *devel-root*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(optimize (debug 3))))
;; End of development only section.

(defun create-empty-branch (module root
			    &key (symbolic-name "") (description ""))
  "Create a new branch. That is to create a branch directory
with the next free number and an empty CHANGE file."
  (in-temporary-directory
    (create-meta-directory)
    (in-meta-directory
      (let ((branch-directory (make-pathname)))
	(retry ((backend-outdated-error
		 :cleanup (progn (delete-file *branch-file*)
				 (delete-directory-tree
				  branch-directory))))
	  (let ((access (create-new-access :root root)))
	    (backend-get module access
			 (list *branch-file* *access-file*) *meta-directory*)
	    (let* ((identifier (get-new-branch-identifier *branch-file*))
		   (branch (make-instance 'branch
			    :identifier identifier
			    :symbolic-name symbolic-name
			    :description description))
		   (change-file (make-pathname)))
	      (setf branch-directory
		    (make-pathname :directory
				   (list :relative
					 (format nil "~A" identifier))))
	      (setf change-file
		    (merge-pathnames branch-directory *change-file*))
	      (add-branch branch *branch-file*)
	      (create-directory branch-directory)
	      (create-new-change-file change-file)
	      (backend-commit module access
			      (list change-file *branch-file*)))))
	(remove-meta-directory)))))

(defun create-empty-repository (module root)
  "Create a completely empty repository only containing an
ACCESS and BRANCH file.
FIXME: It should be checked if module already exists."
  (in-temporary-directory
   (create-meta-directory)
   (in-meta-directory
     (create-new-branch-file)
     (let ((access (create-new-access
		    :identifier 1 :root root)))
       (add-access access *access-file*)
       (backend-import module access)))
   (remove-meta-directory)))