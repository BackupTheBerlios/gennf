;; Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
;;
;; This file is part of port-path.
;;
;; port-path is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; port-path is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gennf; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;;
;; $Id: F-E4E8137ED6A2987D581A8AB6A46FB7DA.lisp,v 1.13 2006/03/14 14:13:11 florenz Exp $

;; Defines the exported function of port-path.

(defpackage :port-path
  (:nicknames :pp)
  (:use :cl)
  (:export
   :change-directory
   :change-directory-up
   :copy-file
   :create-directory
   :create-temporary-directory
   :create-temporary-file
   :current-directory
   :delete-directory-tree
   :directory-listing
   :directory-pathname-p
   :ensure-string-pathname
   :find-all-files
   :get-parent-directory
   :in-directory
   :in-temporary-directory
   :move-directory-tree
   :move-file
   :parent-dirs
   :path-exists-p
   :pathname-prefixes
   :pathname-prefix-p
   :pathname-to-directory-form
   :pathname-to-file-form
   :root-p
   :search-directory-in-directories
   :wild-pathname-p
   :with-directory-form
   :with-file-form
   :with-pathname
   :with-temporary-file)
  (:documentation "A library providing operations related
with pathnames. Many ideas are taken from Peter Seibel's
pathname library."))
