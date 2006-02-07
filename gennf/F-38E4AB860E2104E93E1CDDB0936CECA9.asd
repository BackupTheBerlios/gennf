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
;; $Id: F-38E4AB860E2104E93E1CDDB0936CECA9.asd,v 1.1 2006/02/07 09:59:04 florenz Exp $

(defpackage :gennf-system
  (:use :cl :asdf))

(in-package :gennf-system)
	
(defsystem :gennf
  :name "gennf"
  :author "Hannes Mehnert, Florian Lorenzen, Fabian Otto"
  :version "0.0"
  :maintainer "Florian Lorenzen <florenz@berlios.de>"
  :licence "GPL"
  :description "Distributed version management system with code signing."
  :depends-on (:osicat :port-path)
  :components
  ((:file "packages")
   (:file "create"
	  :depends-on ("packages" "dirwalk" "system" "mapping"
				  "types" "chatter" "options" "restart"))
   (:file "dirwalk"
	  :depends-on ("packages" "system" "restart"))
   (:file "system"
	  :depends-on ("packages" "posix" "sbcl-unix"))
   (:file "posix"
	  :depends-on ("packages"))
   (:file "mapping"
	  :depends-on ("packages" "dirwalk" "system" "options" "sync"
				 "chatter" "restart" "seqfuncs"))
   (:file "sync"
	  :depends-on ("packages" "system"))
   (:file "seqfuncs"
	  :depends-on ("packages" "memoize"))
   (:file "memoize"
	  :depends-on ("packages" "multi-hash"))
   (:file "multi-hash"
	  :depends-on ("packages"))
   (:file "types"
	  :depends-on ("mapping" "system" "print" "seqfuncs"))
   (:file "print"
	  :depends-on ("packages"))
   (:file "chatter"
	  :depends-on ("packages"))
   (:file "checkout"
	  :depends-on ("packages" "dirwalk" "chatter"
				  "sync" "options" "find-bind"))
   (:file "grab"
	  :depends-on ("packages" "dirwalk" "system" "split" "seqfuncs"
				  "mapping" "types" "chatter" "options"
				  "checkout" "remove" "add"))
   (:file "add"
	  :depends-on ("packages" "system" "mapping" "chatter" "dirwalk"
				"seqfuncs" "options" "types"))
   (:file "remove"
	  :depends-on ("packages" "system" "mapping" "chatter" "find-bind"))
   (:file "move"
	  :depends-on ("packages" "system" "mapping" "chatter" "restart"))
   (:file "link"
	  :depends-on ("packages" "system" "mapping" "chatter"))
   (:file "update"
	  :depends-on ("packages" "dirwalk" "chatter" "mapping"
				  "options" "generic"))
   (:file "filt"
	  :depends-on ("packages" "system" "mapping" "options"))
   (:file "generic"
	  :depends-on ("packages" "system" "mapping" "types"
				  "chatter" "options"))
   (:file "convert"
	  :depends-on ("packages" "system" "dirwalk" "chatter"
				  "split" "mapping" "types" "rcs-utils"))
   (:file "rcs-utils"
	  :depends-on ("packages" "slot-refs"))
   (:file "slot-refs"
	  :depends-on ("packages"))
   (:file "branch"
	  :depends-on ("packages" "split" "mapping"
				  "options" "update" "system"))
   (:file "remap"
	  :depends-on ("packages" "dirwalk" "mapping"))
   (:file "purge"
	  :depends-on ("packages" "dirwalk" "mapping" "find-bind"))
   (:file "restore"
	  :depends-on ("packages" "system" "dirwalk" "mapping"))
   (:file "prop"
	  :depends-on ("packages" "system" "mapping" "chatter"))
   (:file "watch"
	  :depends-on ("packages" "generic"))
   (:file "split"
	  :depends-on ("packages"))
   (:file "restart"
	  :depends-on ("packages"))
   (:file "error"
	  :depends-on ("packages" "chatter" "find-bind"))
   (:file "options"
	  :depends-on ("packages" "system" "chatter" "find-bind"
				  "split" "error"))
   (:file "find-bind"
	  :depends-on ("packages"))
   (:file "mcvs-main"
	  :depends-on ("packages" "create" "checkout" "grab" "add"
				  "remove" "move" "link" "update"
				  "filt" "generic" "convert" "branch"
				  "remap" "purge" "restore" "prop"
				  "watch" "split" "restart" "error"
				  "options" "find-bind"))
   (:file "sbcl-unix"
	  :depends-on ("packages" "chatter"))))