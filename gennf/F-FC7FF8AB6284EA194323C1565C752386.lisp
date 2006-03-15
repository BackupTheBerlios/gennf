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
;; $Id: F-FC7FF8AB6284EA194323C1565C752386.lisp,v 1.30 2006/03/15 19:04:13 sigsegv Exp $

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

(define-condition interactive-error ()
  ((text :initarg :text :reader text)))

;; main function, called by shell script
;; Should be put straight with main (see below).
(defun gennf ()
  (format t "Starting GENNF~%")
  (let* ((args (rest (posix-arguments)))
	 (command (first args))
	 (command-args (rest args)))
    (format t "Command:~a~%Arguments:~a~%" command command-args)
    (unless command
      (error 'unknown-subcommand :text "Specify a subcommand!"))
    (dispatch-subcommand command command-args)
    ))
    ;(quit)))				

;;  FIXME: Use global Variable with subcommands and lambdas.
(defun dispatch-subcommand (command command-args)
  (apply (cdr(assoc command *subcommand-list* :test #'string=)) command-args))

(defparameter *subcommand-list* nil)

(defparameter *branch* nil)

(defmacro defsubcommand (subcommand-name (&rest args) &body forms)
  `(progn
     ,(if (and (symbolp (first forms))
	       (eq (first forms) :in-meta-directory))
	  `(defun ,subcommand-name ,args
	     (let ((*meta-directory* (find-meta-directory)))
	       (in-meta-directory
		 (let* ((*branch* (branch (read-sandbox-file)))
			(*map-file* (merge-pathnames
				     (branch-identifier-to-directory *branch*) *map-file*)))
		   ,@(rest forms)))))
	  `(defun ,subcommand-name ,args
	     `(,@forms)))
     (pushnew (cons (format nil "~(~a~)" ',subcommand-name)
		     #',subcommand-name)
	       *subcommand-list* :key #'car :test #'string=)))

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

;; FIXME: multiple adds!
(defsubcommand add (namestring)
  :in-meta-directory
  ;; - find meta-dir
  ;; - create new mapping
  ;; - find mapfile
  ;; - add mapping to file 
  (let* ((id (format nil "META/~a/~a" *branch* (guid-gen)))
	 (path (merge-pathnames (pathname namestring)
				(port-path:get-parent-directory
				 *meta-directory*)))
	 (mapping (create-new-mapping :path path :id id)))
    (add-mapping mapping *map-file*)))

(defsubcommand remove-file (namestring)
  :in-meta-directory
  (let ((mapping (get-mapping namestring *map-file*)))
    (remove-mapping mapping *map-file*)
    (delete-file (path mapping))))

(defsubcommand move (namestring1 namestring2)
  :in-meta-directory
  (let* ((mapping1 (get-mapping namestring1 *map-file*))
	 (path (merge-pathnames (pathname namestring2) 
				(port-path:get-parent-directory *meta-directory*)))
	 (new-mapping (create-new-mapping :kind :file 
					  :path path
					  :id (id mapping1))))
    (remove-mapping mapping1 *map-file*)
    (add-mapping new-mapping *map-file*)
    (sync new-mapping)
    (delete-file (path mapping1))))

;;;; defsubcommand
;;;; =============
;;;; In configuration.lisp werden die globalen Variablen *xxx-file*
;;;; als pathspecs definiert. Sie enthalten aber nur die
;;;; Namenskomponente und sind keine absoluten Pfadangaben und können
;;;; deswegen auch nicht direkt zum öffnen von Dateien verwendet
;;;; werden.

;;;; Fuer die einheitliche Erzeugung von subcommands wäre es von
;;;; Vorteil, wenn es globale Variablen in Namesstyle von *xxx-file*
;;;; gibt. Diese Variablen können dann von dem makro dynamisch
;;;; gesetzt werden.  Mittels dieser File-Variablen wird ein einheitliches
;;;; Interface geschaffen.

;;;; Allerdings ist dafür Voraussetzung, daß die jetzigen Variablen
;;;; in *xxx-file-name* umbenannt werden, um eine
;;;; Namensunabhaengigkeit von GENNF zu erhalten. Diese neuerzeugten
;;;; Variablen koennen dann von den globalen File-Variablen benutzt
;;;; werden.


;; (defsubcommand test-subcommand2  (arg1 arg2 arg3) :in-meta-directory
;; 	       (format t "~a ~a ~a" arg1 arg2 arg3))