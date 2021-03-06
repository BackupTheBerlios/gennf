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
;; $Id: F-48D1C070D93800E7560AEE00EF78D0B2.lisp,v 1.21 2006/04/01 12:45:52 florenz Exp $

;; This file contains various functions and macros that
;; do not fit into any of the other files.
;; If it grows larger, it will be possible to split it in
;; a sensible fashion.

(in-package :gennf)

;;
;; Functions for lists.
;;

(defun delete-sublist (list start end)
  "Returns list without end-start elements from start onwards."
  (append (subseq list 0 start)
	  (subseq list end)))

(defun replace-sublist (list sublist index)
  "Replace list's sublist starting at index with sublist."
  (append (subseq list 0 index)
	  sublist
	  (when (> (length list)
		   (+ index (length sublist)))
	    (subseq list (+ index (length sublist))))))

(defun insert-list (list sublist index)
  "Insert sublist in list at index."
  (append (subseq list 0 index)
	  sublist
	  (subseq list index)))

(defun list-to-string (list &key
		       (convert #'(lambda (element)
				    (format nil "~A" element)))
		       (separator ", ")
		       (open "") (close ""))
  "Convert a list to a string, elements are separated by separator and
converted to strings by convert. open and close are the forst and
the last string elements in the result respectively."
  (format nil "~A~A~A" open
	  (reduce #'(lambda (element1 element2)
		      (format nil "~A~A~A" element1 separator element2))
		  (mapcar convert list))
	  close))
  


;;
;; Functions for alists.
;;

(defun extract (key alist &key (test #'eql))
  "Lookup a an entry in an alist."
  (cdr (assoc key alist :test test)))

(defun unassoc (key alist &key (test #'eql))
  "Remove all entries having key from an alist."
  (remove-if (lambda (pair) (funcall test key (car pair))) alist))

(defun reassoc (key data alist &key (test #'eql))
  "Exchange the value associated with key by data in an alist."
  (acons key data (unassoc key alist :test test)))

(defun alist-union (alist1 alist2 &key (test #'eql))
  "Return the union of all pairs of alist1 and alist2.
No key appears twice in the results. If both alists
contain a certain key, the element of alist2 survives.
If no test for equality is given it defaults to EQL (like ASSOC)."
  (let ((union (copy-alist alist2)))
    (dolist (pair alist1)
      (unless (assoc (car pair) union :test test)
	(setf union (acons (car pair) (cdr pair) union))))
    union))


;;
;; Stuff concerning macros.
;;

(defmacro with-gensyms ((&rest names) &body forms)
  "Generate symbols for all names given to be used in
a macro. Taken from Peter Seibel's book, chapter 8."
  `(let ,(loop for n in names collect `(,n (gensym)))
    ,@forms))


;;
;; Miscellaneous miscellaneous.
;;

(defun search-multiple (sequences sequence)
  "Search all items of sequences in sequences
and return those that are found."
  (let ((results ()))
    (dolist (item sequences)
      (when (search item sequence)
	(push item results)))
    results))


;;
;; Formatting functions.
;; 

(defgeneric log-message-format (entitiy)
  (:documentation "Convert entity into some string suitable for putting
it into a log message. entity may e. g. be an access and is transformed into
its root string and the access method."))

(defgeneric info-format (change)
  (:documentation "Return a string representing the change suitable for
te info subcommand."))
