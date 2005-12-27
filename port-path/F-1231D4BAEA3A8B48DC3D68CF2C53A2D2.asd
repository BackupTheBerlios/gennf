;; Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
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
;; $Id: F-1231D4BAEA3A8B48DC3D68CF2C53A2D2.asd,v 1.1 2005/12/27 16:06:27 florenz Exp $

(defpackage :port-path-system
  (:use :cl :asdf))

(in-package :port-path-system)

(defsystem :port-path
  :name "port-path"
  :author "Hannes Mehnert, Florian Lorenzen, Fabian Otto"
  :version "0.1"
  :maintainer "Florian Lorenzen <florenz@berlios.de>"
  :licence "GPL"
  :description "Some macros and functions to work with pathnames."
  :long-description
"port-path contains some macros and functions to work with pathnames
in Common-Lisp in a portable manner. This library is based on
Peter Seibel's pathnames, which can be found at
http://www.gigamonkeys.com/.

port-path is used in gennf (http://gennf.berlios.de)."
  :components
  ((:file "packages")
   (:file "port-path" :depends-on ("packages"))))