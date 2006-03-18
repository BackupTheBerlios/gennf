;; Copyright 2005 Florian Lorenzen, Fabian Otto
;;
;; This file is part of port-path.
;;
;; port-path is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; port-path is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gennf; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; $Id: F-1231D4BAEA3A8B48DC3D68CF2C53A2D2.asd,v 1.4 2006/03/18 23:31:22 florenz Exp $

;; ASDF definition for port-path.


(defpackage :port-path-system
  (:use :cl :asdf)
  (:documentation "Package for the ASDF definition. This is
ASDF-INSTALL style."))

(in-package :port-path-system)

(defvar *cc* "gcc"
  "C compiler to use for compilation of glue code.
FIXME: This should be done portable somehow.")

(defvar *cc-options* '("-shared" "-fPIC")
  "Options to produce a shared library of the glue code.
FIXME: This should be done more portable.")

(defmethod output-files ((op compile-op) (c c-source-file))
  (list (make-pathname :name (component-name c)
		       :type "so"
		       :defaults (component-pathname c))))

(defmethod perform ((op load-op) (c c-source-file))
  (let ((loader (intern "LOAD-FOREIGN-LIBRARY" :uffi)))
    (dolist (file (asdf::input-files op c))
      (funcall loader file :module "port-path" :force-load t))))

(defmethod perform ((op compile-op) (c c-source-file))
  (unless (zerop (run-shell-command "~A ~A ~{~A ~A~} -o ~A"
				    *cc*
				    (namestring (component-pathname c))
				    *cc-options*
				    (namestring (car (output-files op c)))))
    (error 'operation-error :component c :operation op)))
  
(defsystem :port-path
  :name "port-path"
  :author "Florian Lorenzen, Fabian Otto"
  :version "0.1"
  :maintainer "Florian Lorenzen <florenz@berlios.de>"
  :licence "GPL"
  :description "Some macros and functions to work with pathnames."
  :long-description "port-path contains some
macros and functions to work with pathnames
in Common-Lisp in a portable manner. This library is based on
Peter Seibel's pathnames, which can be found at
http://www.gigamonkeys.com/.
port-path is used in gennf (http://gennf.berlios.de/)."
  :depends-on (:uffi :osicat)
  :components
  ((:file "packages")
   (:file "port-path" :depends-on ("packages" "port-path-posix"))
   (:c-source-file "port-path-posix")))