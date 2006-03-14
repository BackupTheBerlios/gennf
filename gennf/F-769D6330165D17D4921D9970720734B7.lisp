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
;; $Id: F-769D6330165D17D4921D9970720734B7.lisp,v 1.9 2006/03/14 17:36:31 florenz Exp $

;; This file defines global variables which somehow define
;; gennf's configuration.

(in-package :gennf)

;; Debugging.
(defparameter *debug-mode* t
  "Switches on debug mode.")

;; Directories and meta files.
(defparameter *meta-directory-name*
  (make-pathname :directory '(:relative "META"))
  "Name of the meta directory.")
(defparameter *meta-directory* (merge-pathnames *meta-directory-name*)
  "Absolute path of the meta directory.")				
(defparameter *merge-destination*
  (make-pathname :directory '(:relative "destination"))
  "Where files are merged on a merge.")
(defparameter *merge-origin*
  (make-pathname :directory '(:relative "origin"))
  "Where files to be merged in a temporally stored.")
(defparameter *branch-file* (make-pathname :name "BRANCH")
  "Name of file which lists branches.")
(defparameter *access-file* (make-pathname :name "ACCESS")
  "Name of file which lists accesses.")
(defparameter *change-file* (make-pathname :name "CHANGE")
  "Name of file containing the change sequence.")
(defparameter *sandbox-file* (make-pathname :name "SANDBOX")
  "The sandbox file indicates the checked out branch, change and root.")
(defparameter *map-file* (make-pathname :name "MAP")
  "Name of map file.")
(defparameter *update-special-meta-files*
  (list *branch-file* *access-file* *change-file* *sandbox-file*)
  "List of all meta files which have special update handling.")

;; Log messages.
(defparameter *log-empty-branch* "Creation of empty branch."
  "Log message stored when an empty branch is created.")
(defparameter *backend-import-log-message*
  "Creation of a fresh gennf repository.")
