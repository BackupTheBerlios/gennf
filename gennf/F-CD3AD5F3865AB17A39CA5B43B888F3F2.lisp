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
;; $Id: F-CD3AD5F3865AB17A39CA5B43B888F3F2.lisp,v 1.7 2006/02/14 17:32:55 florenz Exp $

;; All directory related functions and macros live in this file.
;; This includes changing working directory, moving and deletion
;; of directory trees.
;; Many of those functions should be generalized as much as possible
;; and go in port-path someday.

(in-package :gennf)

(defmacro in-meta-directory (&body forms)
  "Evaluate forms with *meta-directory* being current
directory and change back to previous working directory
afterwards."
  (let ((current-directory (gensym "current-directory-")))
    `(let ((,current-directory (current-directory)))
      (change-to-meta-directory)
      ,@forms
      (change-directory ,current-directory))))

(defmacro in-temporary-directory ((&optional temporary-pathname) &body forms)
  "Save creation of a temporary directory (e. g. under /tmp),
evaluate forms with temporary directory as working directory,
change back to old working directory and throw away
the temporary directory-tree."
  (with-gensyms (temporary-directory)
    `(let ((,temporary-directory (port-path:create-temporary-directory)))
      (in-directory (,temporary-directory)
       ,(if temporary-pathname
	    `(let ((,temporary-pathname ,temporary-directory))
	      ,@forms)
	    `(progn
	      ,@forms)))
      (delete-directory-tree ,temporary-directory))))

(defmacro in-directory ((directory) &body forms)
  "Evaluate forms in directory and change back
to old working directory afterwards."
  (let ((current-directory (gensym "current-directory-")))
    `(let ((,current-directory (current-directory)))
      (change-directory ,directory)
      ,@forms
      (change-directory ,current-directory))))

(defun create-meta-directory ()
  "Create *meta-directory* and signal a condition, if it
is already there. Set *meta-directory* to path
of the created directory."
  (create-directory *meta-directory-name* :require-fresh-directory t)
  (setf *meta-directory* (merge-pathnames *meta-directory-name*)))

(defun change-directory (pathspec)
  "Change current working directory. Keep the process'
current working directory and *default-pathname-defaults*
synchronized."
  (port-path:with-directory-form ((directory (merge-pathnames pathspec)))
    (if (port-path:path-exists-p directory)
	(progn
	  (setf (osicat:current-directory) directory)
	  (setf *default-pathname-defaults* directory))
	(error "Directory ~S does not exist." directory))))

(defun current-directory ()
  "Return the current working directory."
  *default-pathname-defaults*)

(defun change-directory-up ()
  "Same as cd .. in Unix. If current directory is root
root stays current directory."
  (change-directory (port-path:get-parent-directory (current-directory))))

(defun change-to-meta-directory ()
  "Make *meta-directory* current working directory."
  (change-directory *meta-directory*))

(defun remove-meta-directory ()
  "Delete *meta-directory* and all its subdirectories
and files."
  (when (port-path:path-exists-p *meta-directory*)
    (change-to-meta-directory)
    (change-directory-up)
    (delete-directory-tree *meta-directory*)))

(defun delete-directory-tree (pathspec)
  (port-path:with-directory-form ((directory pathspec))
    (let ((listing (port-path:directory-listing directory)))
      (dolist (entry listing)
	(if (port-path:directory-pathname-p entry)
	    (delete-directory-tree entry)
	    (delete-file entry)))
	(osicat:delete-directory directory))))

(defun create-directory (pathspec &key (require-fresh-directory nil))
  (port-path:with-directory-form ((directory pathspec))
    (let ((absolute-directory (merge-pathnames directory)))
      (multiple-value-bind (path created)
	  (ensure-directories-exist (merge-pathnames absolute-directory))
	(if (and require-fresh-directory (not created))
	    (error "Could not create fresh directory ~S, already existent."
		   absolute-directory)
	    path)))))

(defun find-all-files (pathspec)
  "pathspec is interpreted as a directory and a list
of all files with full pathname below this directory is
returned. That means all pathnames are in file form."
  (port-path:with-directory-form ((directory pathspec))
    (let* ((listing (port-path:directory-listing directory))
	   (files (remove-if #'port-path:directory-pathname-p listing))
	   (directories (remove-if-not #'port-path:directory-pathname-p
				       listing))
	   (file-lists (mapcar #'find-all-files directories)))
      (append files (apply #'append file-lists)))))

(defun move-directory-tree (source-pathspec destination-pathspec)
  "source-pathspec and destination-pathspec are interpreted as
pathnames in directory form. All files and directories below
source-pathspec are moved below destination-pathspec. The
last directory of source-pathspec still exists afterwards."
  (port-path:with-directory-form ((source (merge-pathnames source-pathspec))
				  (destination (merge-pathnames
						destination-pathspec)))
    (let* ((all-sources (find-all-files source))
	   (all-sources-relative
	    (mapcar #'(lambda (file)
			(parse-namestring (enough-namestring file source)))
		    all-sources))
	   (all-destinations
	    (mapcar #'(lambda (file)
			(merge-pathnames file destination))
		    all-sources-relative)))
      (mapcar #'(lambda (source-file destination-file)
		  (ensure-directories-exist destination-file)
		  (move-file source-file destination-file))
	      all-sources all-destinations)
      (mapcar #'delete-directory-tree
	      (port-path:directory-listing source)))))

(defun copy-stream (in out &optional (buffer-size 8192))
  "Copies all data from in to until in is empty.
Streams' elements must be compatible types."
  (unless (subtypep (stream-element-type in) (stream-element-type out))
    (error "Incompatible types of streams' elements."))
  (loop with buffer = (make-array buffer-size
				  :element-type (stream-element-type in))
	for length = (read-sequence buffer in)
	until (= length 0)
	do (write-sequence buffer out :end length)))

(defun copy-file (source destination &key overwrite)
  "Copies a file from source to destination. Overwrites
a possibly existing file if overwrite is T.
T is returned if file was cpoied, NIL otherwise.
source is interpreted as a pathspec in file-form, destination may
be in file- or directory-form. If it is in directory-form
the filename from source is taken.
All directories in destination have to exist."
  (when (port-path:wild-pathname-p source)
    (error "Cannot copy a wild pathname."))
  (when (port-path:wild-pathname-p destination)
    (error "Cannot copy to a wil location."))
  (let* ((from (port-path:pathname-to-file-form source))
	 (to (if (port-path:directory-pathname-p destination)
		 (merge-pathnames
		  (make-pathname
		   :name (pathname-name from)
		   :type (pathname-type from))
		  destination)
		 destination)))
    (with-open-file (in from
			:direction :input
			:if-does-not-exist :error
			:element-type '(unsigned-byte 8))
      (with-open-file (out to
			   :direction :output
			   :if-does-not-exist :create
			   :if-exists (when overwrite :supersede)
			   :element-type '(unsigned-byte 8))
	(when out
	  (copy-stream in out))))))

(defun move-file (source destination &key overwrite)
  "Moves a file from source to destination. If
overwrite is T and destination exists it is overwritten.
move-file returns T if file was actually moved, NIL
otherwise.
Rules from copy-file for source and destination apply.
This move works also for cross file-system moves on
Unix. SBCL's RENAME-FILE e. g. fails in such cases."
  (when (copy-file source destination :overwrite overwrite)
    (delete-file source)))
