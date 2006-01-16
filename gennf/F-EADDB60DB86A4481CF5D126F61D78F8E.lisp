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
;; $Id: F-EADDB60DB86A4481CF5D126F61D78F8E.lisp,v 1.1 2006/01/16 07:47:42 florenz Exp $

(in-package :gennf)

(defun create-new-commit (&key identifier file-map)
  (acons :identifier identifier
	 (acons :file-map file-map ())))

(defun is-commit-p (commit)
  (and (= (length commit) 2)
       (assoc :identifier commit)
       (assoc :file-map commit)))
