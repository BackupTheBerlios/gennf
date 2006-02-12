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
;; $Id: F-663876CA81463AC978FC50C1A85FAFC0.lisp,v 1.2 2006/02/12 19:55:08 florenz Exp $

;; Manipulations of merges.

(in-package :gennf)

(defclass merge (change)
  ((origin :initarg :origin
	   :accessor origin
	   :documentation "The origin of a merge is a reference to
another change."))
  (:documentation "A merge is the result of creating a new change
not with new code but with code from some other change."))

(defun is-merge-p (alist)
  "Test if given alist is a merge representation."
  (and (= (length alist) 3)
       (is-change-p (butlast alist))
       (eql (car (third alist)) :origin)
       (is-origin-p (cdr (third alist)))))

(defmethod convert-to-alist append ((merge merge))
  "Converts a merge to its alist representation."
  (acons :origin (convert-to-alist (origin merge)) ()))

(defclass origin ()
  ((identifier :initarg :identifier
	       :accessor identifier
	       :documentation "Identifier of change referring to.")
   (branch :initarg :branch
	   :accessor branch
	   :documentation "Branch of change referring to")
   (access :initarg :access
	   :accessor access
	   :documentation "Access to get hold of change referring to."))
  (:documentation "An origin established a link between changes."))

(defun is-origin-p (alist)
  "Test if given alist is an origin represenation."
  (and (= (length alist) 3)
       (eql (car (first alist)) :identifier)
       (eql (car (second alist)) :branch)
       (eql (car (third alist)) :access)))

(defmethod convert-to-alist append ((origin origin))
  "Converts an origin object to its alist representation."
  (acons :identifier (identifier origin)
	 (acons :branch (branch origin)
		(acons :access (access origin) ()))))