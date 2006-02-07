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
;; $Id: F-495FEBB82844E29E9B8F3FFCC5A022E2.lisp,v 1.2 2006/02/07 18:05:08 florenz Exp $

;; This file contains routines to manipulate access entries.

(in-package :gennf)

(defun read-access-file ()
  (read-file *access-file*))

(defun write-access-file (sequence)
  (prin1-file *access-file* sequence))

(defun create-new-access (&key (identifier 0) (backend :cvs) root)
  (acons :identifier identifier
	 (acons :backend backend
		(acons :root root ()))))

(defun is-access-p (access)
  (and (= (length access) 3)
       (assoc :identifier access)
       (assoc :backend access)
       (assoc :root access)))

(defgeneric get-new-access-identifier (store))

(defmethod get-new-access-identifier ((file pathname))
  (get-new-access-identifier (read-file file)))

(defmethod get-new-access-identifier ((sequence list))
  (1+ (length sequence)))

(defgeneric add-access (access store))

(defmethod add-access (access (file pathname))
  (prepend-to-list-file file access))

(defmethod add-access (access (sequence list))
  (cons access sequence))

(defgeneric get-access (identifier store))

(defmethod get-access (identifier (file pathname))
  (get-access identifier (read-file file)))

(defmethod get-access (identifier (sequence list))
  (nth identifier sequence))