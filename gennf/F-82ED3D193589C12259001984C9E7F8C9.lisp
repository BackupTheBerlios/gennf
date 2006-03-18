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
;; $Id: F-82ED3D193589C12259001984C9E7F8C9.lisp,v 1.4 2006/03/18 23:37:22 florenz Exp $

;; Simple support for debugging code.


(in-package :gennf)

(defun debug-print (string)
  "Prints string to debug output. Each line in string is
prefixed with *debug-output-prefix*."
  (when *debug-mode*
    (with-input-from-string (stream string)
      (loop for line = (read-line stream nil)
	    while line
	    do (format *debug-io* "~A~A~%" *debug-output-prefix* line)))))

(defun debug-format (&rest arguments)
  "Passes all arguments to format and encloses them
with two lines indicating debug-output. The stream argument
to format is automatically inserted.
Output only in debug mode."
  (when *debug-mode*
    (debug-print (apply #'format (append (list nil) arguments)))))
       
(defmacro debug (&body forms)
  "Executes forms only when in debug-mode. Debug mode is
controlled by *debug-mode*, which is defined in configuration.lisp."
  `(when *debug-mode*
    (progn ,@forms)))
