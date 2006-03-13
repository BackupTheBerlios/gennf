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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.26 2006/03/13 15:07:34 sigsegv Exp $

;; Main module. Basic operations of gennf are implemented in this file.

(in-package :gennf)

;; For development purposes only.
(defparameter *devel-root2*
  "florenz@fiesta.cs.tu-berlin.de:/home/f/florenz/gennf-junk2")
(defparameter *devel-access2*
  (make-instance 'access :root *devel-root2*))
(defparameter *devel-root*
  "florenz@fiesta.cs.tu-berlin.de:/home/f/florenz/gennf-junk")
(defparameter *devel-access*
  (make-instance 'access :root *devel-root*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(optimize (cl:debug 3))))
;; End of development only section.


(defun initialize-repository (module root
			      &key symbolic-name description)
  "Initializes a new repository at location root for the named module.
That means that the repository is created and an initial branch created."
  (let ((access (make-instance 'access :root root)))
    (create-empty-repository module access)
    (create-empty-branch module access :symbolic-name symbolic-name
			 :description description)))

(defun branch-from-change (module access
			   origin-access origin-branch &optional origin-change)
  "Created a new branch for module at access from origin-access
and origin-branch using origin-change if given or the latest.
The identifier of the newly created branch is returned."
  (let ((identifier (create-empty-branch module access)))
    (merge module access identifier
	   origin-access origin-branch origin-change)
  identifier))

;; 
(defun add-file-to-mapping (namestring)
  ;; - find meta-dir
  ;; - create new mapping
  ;; - find mapfile
  ;; - add mapping to file 

  (let ((*meta-directory* (find-meta-directory)))
    (in-meta-directory
      (let* ((branch (branch (read-sandbox-file)))
	     (map-file (merge-pathnames
			(branch-identifier-to-directory branch) *map-file*))
	     (mapping (create-new-mapping :path namestring)))
	(add-mapping mapping map-file)))))
	     
	     


      

				   

      