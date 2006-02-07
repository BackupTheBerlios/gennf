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
;; $Id: F-FE079EED6B0320DCF96CB71FE3B862D6.lisp,v 1.1 2006/02/07 09:59:04 florenz Exp $

(defpackage :gennf
  (:use :cl :osicat :port-path))

(in-package :gennf)


;; FIXME:
;; Those two have to get out of here.
;; Their purpose is to make SBCL happy (define-constant)
;; and to offer a substitute for CLISP's substring.
;; But they belong in some library file or whatever.
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
    ,@(when doc (list doc))))

(defun substring (string begin &optional (end (1- (length string))))
  (coerce (loop for i from begin to end
		collect (aref string i)) 'string))