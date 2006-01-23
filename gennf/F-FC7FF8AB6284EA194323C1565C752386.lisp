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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.4 2006/01/23 18:20:22 florenz Exp $

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
  (in-temporary-directory
    (create-meta-directory)
    (in-meta-directory
      (loop
	  with retry
	  do
	  (setf retry nil)
	  (let ((access (create-new-access :root root)))
	    (backend-get module access
			 (list *branch-file* *access-file*) *meta-directory*)
	    (let* ((identifier (get-new-branch-identifier *branch-file*))
		   (branch (create-new-branch
			    :identifier identifier
			    :symbolic-name symbolic-name
			    :description description))
		   (branch-directory
		    (make-pathname :directory
				   (list :relative
					 (format nil "~A" identifier))))
		   (change-file
		    (merge-pathnames branch-directory *change-file*)))
	      (add-branch branch *branch-file*)
	      (create-directory branch-directory)
	      (create-new-change-file change-file)
	      (handler-case (backend-commit module access
					    (list change-file *branch-file*))
		(backend-outdated-error ()
					(delete-file *branch-file*)
					(delete-directory-tree
					 branch-directory)
					(setf retry t)))))
	  while retry)
      (remove-meta-directory))))
      

(defun create-empty-repository (module root)
  ;; It should be checked if module already exists.
  (in-temporary-directory
   (create-meta-directory)
   (in-meta-directory
     (create-new-branch-file)
     (let ((access (create-new-access
		    :identifier 1 :root root)))
       (add-access access *access-file*)
       (backend-import module access)))
   (remove-meta-directory)))