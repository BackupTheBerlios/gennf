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
;; $Id: F-2B0179C82E79D3FD5CFB9ADB784C71D3.lisp,v 1.3 2006/03/20 01:01:03 florenz Exp $

;; Handling of command line arguments.

(in-package :gennf)

(defmacro defsubcommand (subcommand-name function-name
			 (&rest args) &body forms)
    `(progn
      (let ((*startup-directory* (port-path:current-directory)))
	,(if (and (symbolp (first forms))
		  (eq (first forms) :in-meta-directory))
	     `(defun ,function-name ,args
	       (let* ((*meta-directory* (find-meta-directory))
		      (*sandbox-directory* (port-path:get-parent-directory
					    *meta-directory*)))
		 (in-meta-directory
		   (let* ((sandbox (read-sandbox-file))
			  (*module* (module sandbox))
			  (*branch* (branch sandbox))
			  (*access* (get-access (access sandbox)
						*access-file-name*))
			  (*map-file*
			   (port-path:append-pathnames
			    *meta-directory*
			    (branch-identifier-to-directory *branch*)
			    *map-file-name*)))
		     ,@(rest forms)))))
	     `(defun ,function-name ,args
	       ,@forms))
	(setf *subcommand-list*
	      (reassoc (format nil "~(~a~)" ',subcommand-name)
		       #',function-name *subcommand-list* :test #'string=)))))

(defmacro define-subcommand (subcommand-names function-name
			     (&rest command-arguments) &body forms)
  "Define a gennf subcommand. The command may have more than one name
and is bound to function-name. The arguments are specified via direct
arguments, &key arguments and &rest arguments. Key arguments may
have several names, like --change, -chg, -c. They are specifed
in a list and the first is prefixed with two dashes, all following
with one. The rest parameter must be the last and at only one is
allowed. All arguments from the command line not taken by any other
argument are put in the rest list.
The body may begin with :in-meta-directory, then the directory
context is set and the process put into the meta directory.
The second element of the body may be a docstring, which is
included into the command help string. Also included in the help
string are the command arguments."
  (with-gensyms (arguments parsed-arguments parser)
    (let* ((command-name (format nil "~(~a~)"
				 (if (consp subcommand-names)
				     (first subcommand-names)
				     subcommand-names)))
	   (parameter-list (parse-argument-list command-arguments))
	   ;; Parse the macros forms. This is too simple for
	   ;; a separate parser.
	   (forms1 (if (eql (first forms) :in-meta-directory)
		       (rest forms)
		       forms))
	   (description (if (stringp (first forms1))
			    (first forms1)
			    ""))
	   (forms2 (if (stringp (first forms1))
		       (rest forms1)
		       forms1))
	   ;; Generate the standard-body.
	   (body (if parameter-list
		     `(let* ((,parser
			      ,(generate-argument-parser parameter-list))
			     (,parsed-arguments
			      (funcall ,parser ,arguments))
			     ,@(mapcar
				#'(lambda (variable)
				    `(,(second variable) 
				      (extract ',(second variable)
				       ,parsed-arguments)))
				parameter-list))
		       ,@forms2)
		     `(progn ,@forms2)))
	     (lambda-list (if command-arguments `(&rest ,arguments) ())))
      `(progn
	;; Generate function defintion, depends on the :in-meta-directory flag.
	,(if (eql (first forms) :in-meta-directory)
	     `(defun ,function-name ,lambda-list
	       (let* ((*startup-directory* (port-path:current-directory))
		      (*meta-directory* (find-meta-directory))
		      (*sandbox-directory* (port-path:get-parent-directory
					    *meta-directory*)))
		 (in-meta-directory
		   (let* ((sandbox (read-sandbox-file))
			  (*module* (module sandbox))
			  (*branch* (branch sandbox))
			  (*access* (get-access (access sandbox)
						*access-file-name*))
			  (*map-file*
			   (port-path:append-pathnames
			    *meta-directory*
			    (branch-identifier-to-directory *branch*)
			    *map-file-name*)))
		     ,body))))
	     `(defun ,function-name ,lambda-list
	       ,body))
	;; Register description of subcommand in *subcommand-help*.
	(setf *subcommand-help*
	 (reassoc ,command-name
	  ,(format nil "~A~%~%~A~%"
		   (generate-command-syntax command-name parameter-list)
		   description)
	  *subcommand-help* :test #'string=))
	;; Register subcommand in *subcommand-full-name*.
	(pushnew ,command-name *subcommand-full-names* :test #'string=)
	;; Register subcommand in *subcommand-list*.
	(mapcar
	 #'(lambda (name)
	     (setf *subcommand-list*
		   (reassoc (format nil "~(~A~)" name)
			    #',function-name *subcommand-list*
			    :test #'string=)))
	 (if (listp ',subcommand-names)
	     ',subcommand-names
	     (list ',subcommand-names)))))))

(defun parse-argument-list (argument-list)
  "Parse a define-subcommand argument list."
  (when argument-list
    (let* ((argument (first argument-list))
	   (arguments (rest argument-list)))
      (case argument
	('&key (parse-key-arguments arguments))
	('&rest (parse-rest-arguments arguments))
	(t (parse-plain-arguments argument-list))))))

(defun parse-plain-arguments (argument-list)
  "Parse the plain arguments of a define-subcommand argument list."
  (when argument-list
    (let ((argument (first argument-list))
	  (arguments (rest argument-list)))
      (cond
	((eql argument '&key)
	 (parse-key-arguments arguments))
	((eql argument '&rest)
	 (parse-rest-arguments arguments))
	((symbolp argument)
	 (acons :plain (list argument) (parse-plain-arguments arguments)))
	(t (error "&key, &rest, or symbol expected."))))))

(defun parse-key-arguments (argument-list)
  "Parse the key arguments of a define-subcommand argument list."
  (when argument-list
    (let ((argument (first argument-list))
	  (arguments (rest argument-list)))
      (case argument
	('&rest (parse-rest-arguments arguments))
	(t (append (parse-key-specification argument)
		   (parse-key-arguments arguments)))))))

(defun parse-key-specification (argument)
  "Parse a key description."
  (cond
    ((consp argument) (acons :key argument ()))
    ((symbolp argument) (acons :key (list argument) ()))
    (t (error "List or symbol expected."))))

(defun parse-rest-arguments (argument-list)
  "Parse the rest parameter."
  (cond
    ((= (length argument-list) 1)
     (acons :rest (list (first argument-list)) ()))
    (t (error "Single symbol expected."))))

(defun generate-argument-parser (abstract-syntax)
  "Generate code for a complete command-line specicification. The
input must be from parse-argument-list."
  (let* ((plain-arguments
	  (remove-if-not #'(lambda (element) (eql :plain (car element)))
			 abstract-syntax))
	 (key-arguments
	  (remove-if-not #'(lambda (element) (eql :key (car element)))
			 abstract-syntax))
	 (rest-arguments
	  (remove-if-not #'(lambda (element) (eql :rest (car element)))
			 abstract-syntax))
	 (parse-plain
	  `(parse-plain (token-list)
	    (let ((absy ()))
	      ,@(when plain-arguments
		      (mapcar #'generate-plain-parser-sequence
			      plain-arguments))
	      (append absy (parse-keys token-list)))))
	 (parse-keys
	  `(parse-keys (token-list)
	    (cond
	      ,@(reduce #'append
			(mapcar #'generate-key-parser-case key-arguments))
		(t (parse-rest token-list)))))
	 (parse-rest
	  `(parse-rest (token-list)
	    ,(if rest-arguments
		 `(acons ',(second (first rest-arguments))
		   token-list ())
		 `(when token-list
		   (error "Unknown arguments ~A." token-list))))))
    `#'(lambda (token-list)
	 (labels (,parse-plain ,parse-keys ,parse-rest)
	   (parse-plain token-list)))))

(defun generate-plain-parser-sequence (abstract-syntax)
  "Generate code to parse a certain plain parameter."
  `(progn
    (if token-list
	(progn
	  (setf absy
		(append absy (acons ',(second abstract-syntax)
				    (first token-list) ())))
	  (setf token-list (rest token-list)))
	(error "A ~A is expected." (symbol-name ',(second abstract-syntax))))))
	  
(defun generate-key-parser-case (abstract-syntax)
  "Generate code to parse a certain key parameter."
  (flet ((generate-case (symbol value &optional (long t))
	   (let ((option-string (if long
				    (long-argument-string value)
				    (short-argument-string value))))
	   `((string= (first token-list) ,option-string)
	     (if (second token-list)
		 (append (acons ',symbol (second token-list) ())
			 (parse-keys (nthcdr 2 token-list)))
		 (error "Argument to ~A expected." ,option-string))))))
    (let ((symbol (second abstract-syntax)))
	 (cons (generate-case symbol symbol)
	       (mapcar #'(lambda (value)
			   (generate-case symbol value nil))
		       (nthcdr 2 abstract-syntax))))))

(defun long-argument-string (symbol)
  "Return the long command line switch corresponding to symbol: --symbol."
  (format nil "--~(~A~)" (symbol-name symbol)))

(defun short-argument-string (symbol)
  "Retunr the short command line switch corresponding to symbol: -symbol."
  (format nil "-~(~A~)" (symbol-name symbol)))

(defun generate-command-syntax (command-name parsed-parameter-list)
  "Generate a syntax description for command."
  (let ((syntax (format nil "~A" command-name)))
    (flet ((append-syntax (string)
	     (setf syntax (format nil "~A~A" syntax string))))
      (dolist (parameter parsed-parameter-list)
	(case (first parameter)
	  (:plain (append-syntax (format nil " ~(~A~)" (second parameter))))
	  (:key (append-syntax
		 (format nil " [~A" (long-argument-string (second parameter))))
		(when (> (length parameter) 2)
		  (dolist (key (cddr parameter))
		    (append-syntax 
		     (format nil "|~A" (short-argument-string key)))))
		(append-syntax (format nil " ~(~A~)]" (second parameter))))
	  (:rest (append-syntax (format nil " ~(~A~)..."
					(second parameter)))))))
    syntax))
