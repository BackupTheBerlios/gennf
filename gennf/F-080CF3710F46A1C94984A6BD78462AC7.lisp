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
;; $Id: F-080CF3710F46A1C94984A6BD78462AC7.lisp,v 1.2 2006/02/07 18:05:08 florenz Exp $

;; Manipulation of branches and sequences of branches.

(in-package :gennf)

(defun read-branch-file ()
  "reads a branch file in following format:"
  (read-file *branch-file*))

(defun write-branch-file (sequence)
  (prin1-file *branch-file* sequence))

(defun create-new-branch (&key identifier (symbolic-name "") (description ""))
  (acons :identifier identifier
	 (acons :symbolic-name symbolic-name
		(acons :description description ()))))

(defun is-branch-p (branch)
  (and (= (length branch) 3)
       (assoc :identifer branch)
       (assoc :symbolic-name branch)
       (assoc :description branch)))

(defun create-new-branch-file ()
  (prin1-file *branch-file* ()))

(defgeneric add-branch (branch store))

(defmethod add-branch (branch (file pathname))
  (prepend-to-list-file file branch))

(defmethod add-branch (branch (sequence list))
  (cons branch sequence))

(defgeneric get-new-branch-identifier (store))

(defmethod get-new-branch-identifier ((file pathname))
  (get-new-branch-identifier (read-branch-file)))

(defmethod get-new-branch-identifier ((sequence list))
  (1+ (length sequence)))

(defgeneric get-branch (identifier store))

(defmethod get-branch (identifier (file pathname))
  (get-branch identifier (read-file file)))

(defmethod get-branch (identifier (sequence list))
  (nth identifier sequence))