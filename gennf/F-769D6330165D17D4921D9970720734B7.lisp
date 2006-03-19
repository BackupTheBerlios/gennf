;; Copyright 2006 Florian Lorenzen, Fabian Otto
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
;; $Id: F-769D6330165D17D4921D9970720734B7.lisp,v 1.14 2006/03/19 23:24:26 florenz Exp $

;; This file defines global variables which somehow define
;; gennf's configuration.

(in-package :gennf)

;;
;; Debugging.
;;

(defparameter *debug-mode* t
  "Switches on debug mode.")

(defparameter *debug-output-prefix* "DEBUG: "
  "Each line of debug output is prefixed with this string.")


;;
;; Global variables concering the current context.
;; 

(defparameter *meta-directory* nil
  "Absolute path of current meta directory.")				

(defparameter *sandbox-directory* nil
  "This is the parent-directory of the meta directory. Relative
paths provided by the user are resolved against this directory.")

(defparameter *startup-directory* nil
  "Directory, gennf was started in.")

(defparameter *branch* nil
  "Branch number of current branch.")

(defparameter *map-file* nil
  "Absolute file pathname of the current map file.")

(defparameter *module* ""
  "Current module name.")

(defparameter *access* nil
  "Current access.")


;;
;; Directories and meta files.
;;

(defparameter *meta-directory-name*
  (make-pathname :directory '(:relative "META"))
  "Name of the meta directory.")

(defparameter *merge-destination*
  (make-pathname :directory '(:relative "destination"))
  "Where files are merged on a merge.")

(defparameter *merge-origin*
  (make-pathname :directory '(:relative "origin"))
  "Where files to be merged in a temporally stored.")

(defparameter *branch-file-name* (make-pathname :name "BRANCH")
  "Name of file which lists branches.")

(defparameter *access-file-name* (make-pathname :name "ACCESS")
  "Name of file which lists accesses.")

(defparameter *change-file-name* (make-pathname :name "CHANGE")
  "Name of file containing the change sequence.")

(defparameter *sandbox-file-name* (make-pathname :name "SANDBOX")
  "The sandbox file indicates the checked out branch, change and root.")

(defparameter *map-file-name* (make-pathname :name "MAP" )
  "Name of map file.")

(defparameter *update-special-meta-files*
  (list *branch-file-name* *access-file-name*
	*change-file-name* *sandbox-file-name*)
  "List of all meta files which have special update handling.")


;;
;; Log messages.
;;

(defparameter *log-empty-branch* "Creation of empty branch."
  "Log message stored when an empty branch is created.")

(defparameter *backend-import-log-message*
  "Creation of a fresh gennf repository.")

(defparameter *log-file-name* (make-pathname :name "LOG_MESSAGE")
  "Content of this file is taken as log message on commits.")


;;
;; User interface.
;;

(defparameter *subcommand-list* nil
  "Holds subcommand-names and corresponding functions.")

(defparameter *subcommand-help* nil
  "For each subcommand a description is stored.")