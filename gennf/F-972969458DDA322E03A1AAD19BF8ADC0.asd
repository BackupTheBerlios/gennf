;; 2006 Hannes Mehnert, Florian Lorenzen, Fabian Otto
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
;; $Id: F-972969458DDA322E03A1AAD19BF8ADC0.asd,v 1.13 2006/03/10 13:47:52 sigsegv Exp $

;; ASDF system defintion in ASDF-INSTALL style.


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
  :depends-on (:osicat :port-path :cldoc :cl-difflib)
  :components
  ((:file "packages")
   (:file "gennf"
	  :depends-on ("packages" "distribution"))
   (:file "distribution"
	  :depends-on ("packages" "directories" "configuration" "access"
				  "backend" "branch" "change"
				  "error" "debug"))
   (:file "access"
	  :depends-on ("packages" "configuration" "files"))
   (:file "backend"
	  :depends-on ("packages" "backend-cvs" "miscellaneous" "directories"))
   (:file "backend-cvs"
	  :depends-on ("packages" "miscellaneous" "sbcl" "debug"
				  "configuration" "directories"))
   (:file "merge"
	  :depends-on ("packages" "files" "change"))
   (:file "merging"
	  :depends-on ("packages" "miscellaneous" "files"))
   (:file "branch"
	  :depends-on ("packages" "miscellaneous" "files" "configuration"))
   (:file "change"
	  :depends-on ("packages" "miscellaneous" "files" "configuration"))
   (:file "configuration"
	  :depends-on ("packages"))
   (:file "directories"
	  :depends-on ("packages" "configuration"))
   (:file "files"
	  :depends-on ("packages"))
   (:file "miscellaneous"
	  :depends-on ("packages"))
   (:file "sbcl"
	  :depends-on ("packages"))
   (:file "mapping"
	  :depends-on ("packages" "files" "configuration"))
   (:file "error"
	  :depends-on ("packages" "miscellaneous"))
   (:file "debug"
	  :depends-on ("packages" "configuration"))))
