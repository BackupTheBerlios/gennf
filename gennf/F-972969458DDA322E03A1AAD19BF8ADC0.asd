;; 2006 Florian Lorenzen, Fabian Otto
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
;; $Id: F-972969458DDA322E03A1AAD19BF8ADC0.asd,v 1.24 2006/04/03 23:03:10 florenz Exp $

;; ASDF system defintion in ASDF-INSTALL style.


(defpackage :gennf-system
    (:use :cl :asdf))

(in-package :gennf-system)

(defsystem :gennf
  :name "gennf"
  :author "Florian Lorenzen, Fabian Otto"
  :version "%%VERSION%%"
  :maintainer "Florian Lorenzen <florenz@berlios.de>"
  :licence "GPL"
  :description "Prototype implementaion of distributed version management."
  :depends-on (:osicat :port-path :cl-difflib)
  :components
  ((:file "packages")
   (:file "gennf"
	  :depends-on ("packages" "distribution" "mapping" "configuration"
				  "checkpoint" "command-line" "branch"
				  "change"))
   (:file "distribution"
	  :depends-on ("packages" "directories" "configuration" "access"
				  "backend" "branch" "change" "mapping"
				  "error" "debug" "merging"))
   (:file "access"
	  :depends-on ("packages" "configuration" "files" "miscellaneous"))
   (:file "backend"
	  :depends-on ("packages" "backend-cvs" "miscellaneous" "directories"))
   (:file "backend-cvs"
	  :depends-on ("packages" "miscellaneous" "sbcl" "debug"
				  "configuration" "directories"))
   (:file "merge"
          :depends-on ("packages" "files" "change" "miscellaneous"))
   (:file "merging"
	  :depends-on ("packages" "miscellaneous" "files" "configuration"))
   (:file "branch"
	  :depends-on ("packages" "miscellaneous" "backend"
				  "files" "configuration"))
   (:file "change"
	  :depends-on ("packages" "miscellaneous" "backend" "branch"
				  "files" "configuration" "access"))
   (:file "configuration"
	  :depends-on ("packages"))
   (:file "directories"
	  :depends-on ("packages" "configuration"))
   (:file "files"
	  :depends-on ("packages"))
   (:file "miscellaneous"
	  :depends-on ("packages"))
   (:file "sbcl"
	  :depends-on ("packages" "configuration"))
   (:file "mapping"
	  :depends-on ("packages" "files" "configuration"))
   (:file "error"
	  :depends-on ("packages" "miscellaneous"))
   (:file "debug"
	  :depends-on ("packages" "configuration"))
   (:file "checkpoint"
	  :depends-on ("packages" "files" "configuration" "miscellaneous"))
   (:file "command-line"
	  :depends-on ("packages" "configuration" "miscellaneous"))))
