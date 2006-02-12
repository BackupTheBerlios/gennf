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
;; $Id: F-42DFD065A78C998A3E47C9493FF221E7.lisp,v 1.3 2006/02/12 19:55:08 florenz Exp $

;; Definition of gennf-package (this is ASDF-INSTALL style).

(defpackage :gennf
    (:use :cl)
    (:shadow :merge)
    (:documentation "All functions of gennf are in package gennf.
At a later point, this package may well be splitted into
several ones with distinct functionality."))