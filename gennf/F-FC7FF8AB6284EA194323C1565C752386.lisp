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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.1 2006/01/16 07:47:43 florenz Exp $

(in-package :gennf)

;; For development purposes only.
(defparameter *devel-root*
  "/home/uebb/uebb/infet/admin/florenz/ti/ossi/prototype/repo")
(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(optimize (debug 3))))
;; End of development only section.

(defun create-empty-branch (module root
				   &key (symbolic-name "") (description ""))
  (in-temporary-directory
    (create-meta-directory)
    (in-meta-directory
      (let ((access (create-new-access :root root)))
	(backend-get module access *branch-file* *access-file*))
      (let* ((identifier (get-new-branch-identifier *branch-file*))
	     (branch (create-new-branch
		      :identifier identifier
		      :symbolic-name symbolic-name
		      :description description))
	     (branch-directory
	      (merge-pathnames
	       (make-pathname :directory
			      (list :relative (format nil "~A" identifier))))))
	(add-branch branch *branch-file*)
	(create-directory branch-directory)
	(create-new-change-file (merge-pathnames *change-file*
						 branch-directory))))))

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