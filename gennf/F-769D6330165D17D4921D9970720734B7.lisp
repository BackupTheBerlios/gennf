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
;; $Id: F-769D6330165D17D4921D9970720734B7.lisp,v 1.4 2006/02/13 18:11:14 florenz Exp $

;; This file defines global variables which somehow define
;; gennf's configuration.

(in-package :gennf)

(defparameter *meta-directory-name* (make-pathname :directory
						  '(:relative "META"))
  "Name of the meta directory.")
(defparameter *meta-directory* (merge-pathnames *meta-directory-name*)
  "Absolute path of the meta directory.")				
(defparameter *branch-file* (make-pathname :name "BRANCH"))
(defparameter *access-file* (make-pathname :name "ACCESS"))
(defparameter *change-file* (make-pathname :name "CHANGE"))
(defparameter *backend-import-log-message*
  "Creation of a fresh gennf repository.")
(defparameter *map-file* (make-pathname :name "MAP"))