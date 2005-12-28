;; Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
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
;; $Id: F-DBEF23F4E5EC75C4CF994FA5C7DE0326.lisp,v 1.2 2005/12/28 16:20:40 florenz Exp $


;; This file is the equivalent to clisp-unix.lisp and cmucl-unix.lisp
;; for the SBCL implementation.
;; Many parts are already portable but some rely on SBCL specific
;; extensions and I am not sure if it is possible to write
;; process interaction via stream in a portable fashion.
;;
;; The error handling in this file is weaker than in clisp-unix.lisp
;; because no special conditions are written.
;; This can be added in the future but has to be done differently than
;; in clisp-unix.lisp because that code relies on non-standard CLISP
;; behaviour to call :after methods on make-condition.
;; Anyway, it seems more promising to write the condition handling code
;; when oddities like pathnames as strings are removed.
;; Having that another set of routines will be necessary, which will
;; be much smaller.
;; At the moment, all functions and macros from clisp-unix.lisp and
;; cmucl-unix.lisp are just replaced by either portable or SBCL
;; specific ones.


;(defpackage :sbcl-unix
;  (:use :cl :port-path))

(in-package :gennf)


;;; Terminal

(defun ctermid ()
  "function CTERMID => string

Returns name of process' terminal."
  (osicat:terminal-id))


(defmacro no-existence-error (&body forms)
  "macro NO-EXISTANCE-ERROR &bodey forms

Macro to catch ENOENT errors and turn them into nil 
return value."
  (let ((block-sym (gensym "BLOCK-")))
    `(block ,block-sym
      (handler-bind
	  ((system-error #'(lambda (con)
			     (declare (ignore con))
			     (if (= (osicat:unix-error) 2)
				 (return-from ,block-sym nil)))))
	,@forms))))


;;; Functions for files.
;;;
;;; The following functions should work for all Common Lisp
;;; implementations because they use osicat and port-path.

(defclass file-info ()
  ((file-name :initarg :file-name :accessor file-name)
   (mode-flags :initarg :mode-flags :accessor mode-flags)
   (mod-time :initarg :mod-time :accessor mod-time)
   (inode :initarg :inode :accessor inode)
   (num-links :initarg :num-links :accessor num-links))
  (:documentation
"file-info contains the most important information for a
particular file."))


(defun stat (name &key through-link)
  "function STAT name &key through-link => file-info

Return a file-info for name. If through-link is non-nil
and name a symbolic link a file-info for the link's
target is returned."
  (if (typep name 'file-info)
      name
      (with-pathname ((pathname name))
	(make-instance
	 'file-info
	 :file-name (namestring pathname)
	 :mode-flags (osicat:file-mode pathname through-link)
	 :mod-time (osicat:file-modification-time pathname through-link)
	 :inode (osicat:inode-number-of-file pathname through-link)
	 :num-links (osicat:number-of-links-to-file
		     pathname through-link)))))


(defun readdir (directory)
  "function READDIR directory => list

Returns a directory listing of directory as a list of strings."
  (with-pathname ((pathname directory))
    (mapcar #'namestring (directory-listing pathname))))


(defun chdir (directory)
  "function CHDIR directory

Change current working directory."
  (with-pathname ((pathspec directory))
    (setf (osicat:current-directory) pathspec)))


(defun getcwd ()
  "function GETCWD => string

Get current working directory."
  (osicat:current-directory))


(defgeneric same-file-p (file1 file2)
  (:documentation
"generic function SAME-FILE-P file1 file2 => generalized boolean

Check if the two files file1 and file2 are the same, i. e.
have the same inode number."))


(defmethod same-file-p ((file1 string) (file2 string))
  (with-pathname ((pathname1 file1) (pathname2 file2))
    (= (osicat:inode-number-of-file pathname1)
       (osicat:inode-number-of-file pathname2))))


(defmethod same-file-p ((file1 file-info) (file2 file-info))
  (= (inode file1) (inode file2)))


(defgeneric older-p (file1 file2)
  (:documentation
"generic function OLDER-P file1 file2 => generalized boolean

Checks if file1 is older than file2."))


(defmethod older-p ((file1 string) (file2 string))
  (with-pathname ((pathname1 file1) (pathname2 file2))
    (< (file-write-date pathname1)
       (file-write-date pathname2))))


(defmethod older-p ((file1 file-info) (file2 file-info))
  (< (mod-time file1) (mod-time file2)))


(defgeneric regular-p (file)
  (:documentation
"generic function REGULAR-P file => generalized boolean

Checks if file is a regular file."))


(defmethod regular-p ((file string))
  (with-pathname ((pathname file))
    (eql :regular-file
	 (osicat:file-kind pathname))))


(defmethod regular-p ((file file-info))
  (regular-p (file-name file)))


(defgeneric directory-p (file)
  (:documentation
"generic function DIRECTORY-P file => generalized boolean

Checks if file is a directory."))


(defmethod directory-p ((file string))
  (with-pathname ((pathname file))
    (eql :directory
	 (osicat:file-kind pathname))))


(defmethod directory-p ((file file-info))
  (directory-p (file-name file)))


(defgeneric symlink-p (file)
  (:documentation
"generic function SYMLINK-P file => generalized boolean

Checks if file is a symbolic link."))


(defmethod symlink-p ((file string))
  (with-pathname ((pathname file))
    (eql :symbolic-link
	 (osicat:file-kind pathname))))


(defmethod symlink-p ((file file-info))
  (symlink-p (file-name file)))


(defgeneric is-root-p (file)
  (:documentation
"generic function IS-ROOT-P file => generalized boolean

Checks if file is the file system root."))


(defmethod is-root-p ((file string))
  (with-pathname ((pathname file))
    (root-p pathname)))


(defmethod is-root-p ((file file-info))
  (is-root-p (file-name file)))


(defgeneric get-parent (file)
  (:documentation
"generic function GET-PARENT file => string

Returns the parent directory of file as a string."))


(defmethod get-parent ((file string))
  (with-pathname ((pathname file))
    (namestring (get-parent-directory pathname))))


(defmethod get-parent ((file file-info))
  (get-parent (file-name file)))


(defgeneric executable-p (file)
  (:documentation
"generic function executable-p file => generalized boolean

Checks if file is executable, i. e. owner and
group have execute permissions."))


(defmethod executable-p ((file string))
  (with-pathname ((pathname file))
    (let ((permissions (osicat:file-permissions pathname)))
      (and
       (find :user-exec permissions)
       (find :group-exec permissions)))))


(defmethod executable-p ((file file-info))
  (executable-p (file-name file)))


(defgeneric make-executable (file)
  (:documentation
"generic function MAKE-EXECUTABLE file

Sets owner, group and other execute permissions
if owner, group, other have read-permissions respectively."))


(defmethod make-executable ((file string))
  (with-pathname ((pathname file))
    (flet ((insert (element list)
	     (if (find element list)
		 list
		 (cons element list))))
      (let ((permissions (osicat:file-permissions pathname)))
	(when (find :user-read permissions)
	  (setf permissions (insert :user-exec permissions)))
	(when (find :group-read permissions)
	  (setf permissions (insert :group-exec permissions)))
	(when (find :other-read permissions)
	  (setf permissions (insert :other-exec permissions)))
	(setf (osicat:file-permissions pathname) permissions)))))


(defmethod make-executable ((file file-info))
  (make-executable (file-name file)))


(defgeneric make-non-executable (file)
  (:documentation
"generic function MAKE-NON-EXECUTABLE file

Removes all executable bits from file."))


(defmethod make-non-executable ((file string))
  (with-pathname ((pathname file))
    (let ((permissions (osicat:file-permissions pathname)))
      (mapcar (lambda (p)
	     (setf permissions (remove p permissions)))
	   '(:user-exec :group-exec :other-exec))
      (setf (osicat:file-permissions pathname) permissions))))


(defmethod make-non-executable ((file file-info))
  (make-non-executable (file-name file)))


(defun exists (file &key through-link)
  "function EXISTS file &key through-link => generalized boolean

Checks if file exists."
  (with-pathname ((pathname file))
    (let ((kind (osicat:file-kind pathname)))
      (if (and through-link (eql kind :symbolic-link))
	  ; Pathnames have to be merged because the result of
	  ; osicat:read-link may be relative to pathname.
	  (osicat:file-kind
	   (merge-pathnames (osicat:read-link pathname) pathname))
	  kind))))


(defun link (old-name new-name)
  "function LINK old-name new-name

new-name is a new name (hard link) for old-name."
  (with-pathname ((old-pathname old-name) (new-pathname new-name))
    (osicat:make-link new-pathname :target old-pathname :hard t)))


(defun symlink (file link)
  "function SYMLINK name link

Creates a symbolic link named link to file."
  (with-pathname ((file-pathname file) (link-pathname link))
    (osicat:make-link link-pathname :target file-pathname)))


(defun readlink (symbolic-link)
  "function READLINK pathspec => string

Returns the target of symbolic link pathspec."
  (with-pathname ((pathname symbolic-link))
    (osicat:read-link pathname)))


(defun rmdir (directory)
  "function RMDIR directory

Deletes directory, which must be empty."
  (with-pathname ((pathname directory))
    (osicat:delete-directory pathname)))


(defun unlink (file)
  "function UNLINK file

Delete file."
  (with-pathname ((pathname file))
    (delete-file pathname)))


(defun env-lookup (name &optional substitute-if-not-found)
  "function ENV-LOOKUP name &optional substitute-if-not-found

Return the value of environment variable name. If name is
an undefined environment variable substitute-if-not-found
is returned if defined."
  (let ((value (osicat:environment-variable name)))
    (if value value substitute-if-not-found)))


(defmacro current-dir-restore (&body forms)
  "macro CURRENT-DIR-RESTORE &body forms

(getcwd) before and after execution of forms is
the same as long as (getcwd) is not deleted in forms (this
causes an error).

The local macro 

IN-ORIGINAL-DIR &body inner-forms

may be used in forms, which does the same job as CURRENT-DIR-RESTORE
but ensures that inner-forms are executed in the directory,
which was current before entrance of forms."
  (let ((current-dir (gensym "current-dir-")))
    `(exists (getcwd))
    `(let ((,current-dir (getcwd)))
      (unwind-protect
	   (macrolet ((in-original-dir (&body inner-forms)
			(let ((inner-dir (gensym "inner-dir-")))
			  `(exists (getcwd))
			  `(let ((,inner-dir (getcwd)))
			    (unwind-protect
				 (progn
				   (chdir ,',current-dir)
				   (progn ,@inner-forms))
			      (chdir ,inner-dir))))))
	     ,@forms)
	(chdir ,current-dir)))))
      

;;; GUID generation.

(defvar *have-dev-random* t)
(defvar *mcvs-random-state*)

(defun guid-gen ()
  (cond
    (*have-dev-random*
     (or (ignore-errors 
	   (with-open-file (f "/dev/urandom" 
			      :direction :input 
			      :element-type '(unsigned-byte 128))
	     (read-byte f)))
	 (progn
	   (setf *have-dev-random* nil)
	   (setf *mcvs-random-state* (make-random-state t))
	   (guid-gen))))
    (t (random #.(expt 2 128) *mcvs-random-state*))))


;;; Interaction with other processes.
;;;
;;; The following code is specific to SBCL.

(defun shell-interpreter (command)
  "function SHELL-INTERPRETER command

Runs the user's shell (expects an environment variable USER)
with command as arguments to the shell's -c option.
If the user's shell can not be determined, /bin/sh is
started.

This function only works with SBCL."
  (let* ((user (osicat:environment-variable "USER"))
	 (user-shell (cdr (assoc :shell
				 (osicat:user-info user))))
	 (shell (if user-shell user-shell "/bin/sh")))
    (sb-ext:run-program shell
			`("-c" ,command))))


(defun execute-program (arguments)
  "function EXECUTE-PROGRAM arguments => generalized boolean

Runs arguments. The first element in arguments is
the program to run and the subsequent is its parameters.

Returns T if program terminated successfully, NIL otherwise.

This function only works with SBCL."
  (chatter-debug "invoking ~s in directory ~s~%" arguments (getcwd))
  (let ((process (sb-ext:run-program (first arguments)
				     (rest arguments))))
    (if (and (eql (sb-ext:process-status process) :exited)
	       (= (sb-ext:process-exit-code process) 0))
	(progn
	   (chatter-debug "successful termination~%")
	   T)
	(progn
	  (chatter-debug "unsuccessful or abnormal termination~%")
	  nil))))
      
      
  

(defmacro with-input-from-program ((stream arguments) &body forms)
  "macro WITH-INPUT-FROM-PROGRAM (stream arguments) &body forms.

Runs program designated by arguments, which is a quoted list
and its first element is the programname and the subsequent
is its arguments. The output is directed to stream which an
be used in forms.

This macro only works with SBCL."
  ; All that quoteing and unquoteing is just because arguments
  ; comes as a quoted list. This is for compatibility reasons with
  ; the original Meta-CVS code but I would like to remove it later.
  (let ((unquoted-arguments arguments))
    `(let* ((process (sb-ext:run-program (first ,unquoted-arguments)
					 (rest ,unquoted-arguments)
					 :output :stream))
	    (,stream (sb-ext:process-output process)))
      (unwind-protect
	   (progn ,@forms)
	(when (sb-ext:process-p process)
	  (sb-ext:process-close process))))))


(defmacro with-output-to-program ((stream arguments) &body forms)
  "macro WITH-OUTPUT-TO-PROGRAM (stream arguments) &body forms.

Runs program designated by arguments, which is a quoted list
and its first element is the programname and the subsequent
is its arguments. The program's standard input is connected
to stream, which is flushed after execution of forms.

This macro only works with SBCL."
  (let ((unquoted-arguments arguments))
    `(let* ((process (sb-ext:run-program (first ,unquoted-arguments)
					 (rest ,unquoted-arguments)
					 :input :stream
					 :wait nil))
	    (,stream (sb-ext:process-input process)))
      (unwind-protect
  	   (progn ,@forms)
	(force-output ,stream)
	(when (sb-ext:process-p process)
	  (sb-ext:process-close process))))))