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
;; $Id: F-663876CA81463AC978FC50C1A85FAFC0.lisp,v 1.1 2006/02/08 19:59:49 florenz Exp $

;; Manipulations of merges.

(in-package :gennf)

(defclass merge (change)
  ((origin :initarg :origin
	   :accessor origin
	   :documentation "The origin of a merge is a reference to
another change."))
  (:documentation "A merge is the result of creating a new change
not with new code but with code from some other change."))