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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.47 2006/04/01 12:45:52 florenz Exp $

;; Main module. Subcommands of gennf are implemented in this file.

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
	(quit 0))
    (error (condition)
      (progn
	(error-output "~%~A~%~%" condition)
	(quit 1)))))

(defun dispatch-subcommand (command command-arguments)
  (apply (extract command *subcommand-list* :test #'string=)
	 command-arguments))

;;
;; Subcommands.
;; 

(define-subcommand help subcommand-help (&rest command)
  "Show some help messages or, if command is given, help about that
command."
  (if command
      (format t "~A~%"
	      (extract (first command) *subcommand-help* :test #'string=))
      (progn
	(format t "The following commands are available:~%")
	(dolist (subcommand *subcommand-full-names*)
	  (format t "~A~%" subcommand)))))


(define-subcommand add subcommand-add (&rest files)
  ;; FIXME: multiple adds!
  :in-meta-directory
  "Add files to versioned files."
  (port-path:in-directory *sandbox-directory*
    (when (null files)
      (format t "~%You must atleast specify one file~%")
      (return-from subcommand-add))
    (dolist (namestring files)
      (let* ((id (format nil "~A" (guid-gen)))
	     (path (pathname namestring))
	     (mapping (create-new-mapping :path path :id id)))
	(add-mapping mapping *map-file*)
	(subcommand-sync)))
    (format t "~{~a ~}~%" files)))

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
    (module &key (:required root r) (branch b) (change c))
  "Check out the indicated branch. If no branch is given 1 is taken."
  (let ((working-directory
	 (merge-pathnames (make-pathname :directory (list :relative module))))
	(access (make-instance 'access :root root)))
    (ensure-directories-exist working-directory)
    (port-path:in-directory working-directory
      (distribution-checkout module access
			     (if branch (parse-integer branch) 1)
			     (if change (parse-integer change)))
      (subcommand-sync))))

(define-subcommand setup subcommand-setup
    (module &key (:required root r) (symbolic-name n) (description d))
  "Setup a new module."
  (let ((access (make-instance 'access :root root)))
    (create-empty-repository module access)
    (create-empty-branch module access :symbolic-name symbolic-name
			 :description description)
    (format t "The new module can be checked out with:~%")
    (format t "$ gennf checkout ~A --root ~A~%" module root)))

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
    (subcommand-sync)))

(define-subcommand (branch br) subcommand-branch
    (module &key (:required root-from f) (:required root-to t)
	    (branch b) (change c))
  (let* ((source (make-instance 'access :root root-from))
	 (destination (make-instance 'access :root root-to)))
    ;; FIXME: check the correct access
    (format t "**** Checking for existing module ~a in destination ~a.~%"
	    module root-to)
    (unless (backend-known-module-p module destination)
      (format t "**** Creating new module ~A at ~A.~%" module root-to)
      (create-empty-repository module destination))
    (format t "**** Creating empty branch.~%")
    (let ((identifier (create-empty-branch module destination)))
      (distribution-merge module destination identifier
			  source (parse-integer branch)
			  (if change (parse-integer change)))
      (format t "**** The branch can be checked out with:~%")
      (format t "$ gennf checkout ~A -r ~A -c ~A~%"
	      module root-from identifier))))


(define-subcommand (merge mg) subcommand-merge
    (module &key (:required root-from f) (:required branch-from b)
	    (:required root-to t) (:required branch-to d) (change c))
  "Merge branch-from into branch-to."
  (let* ((source (make-instance 'access :root root-from))
	 (destination (make-instance 'access :root root-to)))
    (multiple-value-bind (destination-directory files) 
	(distribution-merge module destination branch-to source
			    branch-from change)
      (unless destination-directory 
	(format t "merge finished cleanly.")
	(return-from subcommand-merge))
      ;; Handling CONFLICT, writing CHECKPOINT.
      (let* ((*meta-directory* destination-directory)
	     (*sandbox-directory* (port-path:get-parent-directory
				   *meta-directory*)))
	(in-meta-directory
	  (let* ((branch-directory (branch-identifier-to-directory branch-to))
		 (map-file (port-path:append-pathnames *meta-directory*
						       branch-directory
						       *map-file-name*)))
	    (write-checkpoint-file
	     (make-instance 'checkpoint :files files :branch branch-to
			    :module module :root root-to)
	     *checkpoint-file-name*)
	    (handler-case 
		(let* ((mapping-list (read-map-file map-file))
		       (existing-mapping-list
			(remove-if-not
			 #'(lambda (m) 
			     (port-path:path-exists-p
			      (port-path:append-pathnames *meta-directory*
							  branch-directory
							  (id m))))
			 mapping-list)))
		  (sync-mappings existing-mapping-list branch-directory))
	      (malformed-map-file-error () ; catching Error.
		(format t "WARNING: Due to merging conflicts the MAP file is broken~%")
		(format t "WARNING: MAP file in ~A is malformed~%"
			destination-directory)
		(format t "WARNING: Please fix the broken MAP file.~%")
		(format t "WARNING: gennf will otherwise *not* produce human readable named files.~%")
		(return-from subcommand-merge)))))))))

(define-subcommand sync subcommand-sync (&rest path)
  "Syncs current directory or the specified path."
  (let* ((raw-directory (if (null path)
			    (port-path:current-directory)
			    (first path))))
    (port-path:with-directory-form ((directory raw-directory))
      (format t "Syncing in directory ~a~%" directory)
      (if (port-path:directory-pathname-p directory)
	  (port-path:in-directory directory
	    (let*  ((*startup-directory* (port-path:current-directory))
		    (*meta-directory* (find-meta-directory))
		    (*sandbox-directory* (port-path:get-parent-directory
					  *meta-directory*)))
	      (in-meta-directory
		(let* (meta-file)
		  (if (port-path:path-exists-p *checkpoint-file-name*)
		      (setf meta-file (read-checkpoint-file))
		      (setf meta-file (read-sandbox-file)))
		  (let* ((*module* (module meta-file))
			 (*branch* (branch meta-file))
			 (*map-file*
			  (port-path:append-pathnames
			   *meta-directory*
			   (branch-identifier-to-directory *branch*)
			   *map-file-name*)))
		    (sync-mappings *map-file*
				   (branch-identifier-to-directory
				    *branch*)))))))))))

(define-subcommand info subcommand-info
    (&key (:flag verbose v) (module m) (root r) (branch b))
  "Reports information about a branch."
  (let (access)
    (if (or root module branch)
	(progn
	  (unless (and root module)
	    (error "--module and --root, have to be provided, because no META directory could be found."))
	  (setf access (make-instance 'access :root root)))
	(let ((*meta-directory* (find-meta-directory)))
	  (in-meta-directory
	    (let ((sandbox (read-sandbox-file)))
	      (setf branch (branch sandbox))
	      (setf module (module sandbox))
	      (setf access (get-access (access sandbox)
				       *access-file-name*))))))
    (let* ((latest-branches (retrieve-latest-branch module access)))
      (format t "The module ~A has the following branches:~%~%"
	      module)
      (dolist (branch latest-branches)
	(format t "~A~%~%" (info-format branch))
	(when verbose
	  (format t "This branch has the following changes:~%~%")
	  (dolist (change (retrieve-all-changes module access
						(identifier branch)))
	    (format t "~A~%" (info-format change)))
	  (format t "~%~%"))))))

(define-subcommand merge-finish subcommand-merge-finish (&rest path)
  "continues a merge with conflicts."
  (let* ((raw-directory (if (null path)
			    (port-path:current-directory)
			    (first path))))
    (port-path:with-directory-form ((directory raw-directory))
      (format t "~%*** Continuing in directory ~a~%" directory)
      (if (port-path:directory-pathname-p directory)
	  (port-path:in-directory directory
	    (let* ((*startup-directory* (port-path:current-directory))
		   (*meta-directory* (find-meta-directory))
		   (*sandbox-directory* (port-path:get-parent-directory
					 *meta-directory*)))
	      (format t "*s-d*: ~s~%*m-d*: ~s~%*s-d*: ~s~%" 
		      *startup-directory* *meta-directory* *sandbox-directory*)
	      (in-meta-directory
		(let* ((checkpoint (read-checkpoint-file
				    *checkpoint-file-name*))
		       (*branch* (branch checkpoint))
		       (root (root checkpoint))
		       (module (module checkpoint))
		       (files (files checkpoint))
		       (*access* (make-instance 'access :root root)))
		  (distribution-merge-finish module *branch* *access*
					     *sandbox-directory* files)
		  ;; Removing temporary directory
		  ;; (two levels above the sandbox).
		  (pp:delete-directory-tree 
		   (port-path:get-parent-directory
		    (port-path:get-parent-directory
		     *sandbox-directory*)))))))))))
