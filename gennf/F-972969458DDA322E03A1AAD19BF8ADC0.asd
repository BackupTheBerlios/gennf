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
;; $Id: F-972969458DDA322E03A1AAD19BF8ADC0.asd,v 1.1 2006/01/24 14:14:01 florenz Exp $

(defpackage :gennf-system
    (:use :cl :asdf))

(in-package :gennf-system)

(defsystem :gennf
  :name "gennf"
  :author "Hannes Mehnert, Florian Lorenzen, Fabian Otto"
  :version "0.0"
  :maintainer "Florian Lorenzen <florenz@berlios.de>"
  :licence "GPL"
  :description "Prototype implementaion of distributed version management."
  :depends-on (:osicat :port-path)
  :components
  ((:file "packages")
   (:file "gennf"
	  :depends-on ("packages" "directories" "configuration" "access"
				  "backend" "branch" "change"))
   (:file "access"
	  :depends-on ("packages" "configuration" "files"))
   (:file "backend"
	  :depends-on ("packages" "backend-cvs" "miscallenous" "directories"))
   (:file "backend-cvs"
	  :depends-on ("packages" "miscallenous" "sbcl" "configuration"))
   (:file "branch"
	  :depends-on ("packages" "miscallenous" "files" "configuration"))
   (:file "change"
	  :depends-on ("packages" "miscallenous" "files" "configuration"))
   (:file "commit"
	  :depends-on ("packages" "miscallenous" "files"))
   (:file "configuration"
	  :depends-on ("packages"))
   (:file "directories"
	  :depends-on ("packages" "configuration"))
   (:file "files"
	  :depends-on ("packages"))
   (:file "miscallenous"
	  :depends-on ("packages"))
   (:file "sbcl"
	  :depends-on ("packages"))))