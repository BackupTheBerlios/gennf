;; Copyright 2005 Hannes Mehnert, Florian Lorenzen, Fabian Otto
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
;; $Id: F-30CE3B42F628E6E185661335E4018306.lisp,v 1.2 2005/12/02 11:11:40 florenz Exp $

;; Common Lisp gpgme-wrapper
;; Date: Thu Dec  1 17:26:32 MET 2005
;;
;; This wrapper is used in our meta-cvs patch (gennf),
;; for signinig code hunks/sets. 

;; Example FFI call
;; (ffi:def-call-out dot-product
;;          (:name "dot_product")
;;          (:language :stdc)
;;          (:arguments
;;           (a (ffi:c-ptr (ffi:c-array single-float 3)))
;;           (b (ffi:c-ptr (ffi:c-array single-float 3))))
;;          (:return-type single-float))


