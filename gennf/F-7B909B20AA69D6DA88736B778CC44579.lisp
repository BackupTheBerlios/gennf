;; 2006 Hannes Mehnert, Florian Lorenzen, Fabian Otto
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
;; $Id: F-7B909B20AA69D6DA88736B778CC44579.lisp,v 1.3 2006/02/16 13:37:18 florenz Exp $


;; Computing differences of files and merging them.
;; Note: this file is not related to a merge as part
;; of a change sequence.

(in-package :gennf)

(defgeneric differences (sequence1 sequence2 &key equality)
  (:documentation "Computes differences between sequence1 and
sequence2. If provided, equality is used to test
the sequences' elements for equality. If not, the default equality is
method dependent."))

(defmethod differences ((list1 list) (list2 list)
                        &key (equality #'equal))
  "Computes differences between list1 and list2. If provided
equality is used to test element equality."
  (let* ((matcher (make-instance 'difflib:sequence-matcher
                                 :a list2
                                 :b list1
                                :test-function equality)))
    (difflib:get-opcodes matcher)))

(defmethod differences ((file1 pathname) (file2 pathname)
                        &key (equality #'equal))
  "Computes differences between file1 and file2."
  (differences (file-to-list file1) (file-to-list file2)
               :equality equality))

(defun conflictp (differences)
  "Returns wheather the differences indicate any
conflicts."
  (/= 0 (count-if #'(lambda (element)
		      (or (eql (difflib:opcode-tag element) :replace)
			  (eql (difflib:opcode-tag element) :delete)))
		  differences)))

(defgeneric two-way-merge (sequence1 sequence2
                                     &key equality conflicting
				     delete-markup-function
				     replace-markup-function)
  (:documentation "Performs a two-way merge: sequence1 is merged into
sequence2. If provided, equality is used in computation of the
differences. If conflicting is T the result may contain
conflicts and corresponding inidicators. If conflicting is NIL,
the merge is performed only if no conflicts arise.
The result is a multiple-value: the first value is the merged
sequence and the second value indicates if conflicts arose."))

(defmethod two-way-merge ((list1 list) (list2 list)
			  &key (equality #'equal) conflicting
			  delete-markup-function replace-markup-function)
  "Merges list1 into list2 and returns the result."
  (let* ((differences (differences list1 list2 :equality equality))
	 (conflicts (conflictp differences)))
    (if (and conflicts (not conflicting))
	(values () conflicts)
	(values (perform-two-way-merge
		 differences list1 list2
		 :delete-markup-function delete-markup-function
		 :replace-markup-function replace-markup-function)
		conflicts))))

(defmethod two-way-merge ((file1 pathname) (file2 pathname)
			  &key (equality #'equal) conflicting
			  delete-markup-function replace-markup-function)
  "Merges file1 into fil2 and returns the result as a list."
  (two-way-merge (file-to-list file1)
		 (file-to-list file2)
		 :equality equality
		 :conflicting conflicting
		 :delete-markup-function delete-markup-function
		 :replace-markup-function replace-markup-function))

(defun default-delete-markup (deletion)
  "Standard marker function for delete conflicts."
  (append
   '("<<< The part below is missing in list1 <<<")
   deletion
   '(">>> The part above is missing in list1 >>>")))

(defun default-replace-markup (chunk-of-list1 chunk-of-list2)
  "Standard marker function for replace conflicts."
  (append
   '("<<< The part below is from list1 <<<")
   chunk-of-list1
   '(">>> The part above is from list1 >>>")
   '("<<< The part below is from list2 >>>")
   chunk-of-list2
   '(">>> The part above is from list2 >>>")))

(defun perform-two-way-merge (differences list1 list2
			      &key (delete-markup-function
				    #'default-delete-markup)
			      (replace-markup-function
			       #'default-replace-markup))
  "Computes a merge of list1 into list2 using differences.
Conflict markers are produced by delete-markup-function and
replace-markup-functions."
  (let ((merge ()))
    (dolist (difference differences)
      (cond ((eql (difflib:opcode-tag difference) :equal)
	     (setf merge
		   (append merge
			   (subseq list1
				   (difflib:opcode-j1 difference)
				   (difflib:opcode-j2 difference)))))
	    ((eql (difflib:opcode-tag difference) :insert)
	     (setf merge
		   (append merge
			   (subseq list1
				   (difflib:opcode-j1 difference)
				   (difflib:opcode-j2 difference)))))
	    ((eql (difflib:opcode-tag difference) :delete)
	     (setf merge
		   (append merge
			   (funcall
			    delete-markup-function
			    (subseq list2
				    (difflib:opcode-i1 difference)
				    (difflib:opcode-i2 difference))))))
	    ((eql (difflib:opcode-tag difference) :replace)
	     (setf merge
		   (append merge
			   (funcall
			    replace-markup-function
			    (subseq list1
				    (difflib:opcode-j1 difference)
				    (difflib:opcode-j2 difference))
			    (subseq list2
				    (difflib:opcode-i1 difference)
				    (difflib:opcode-i2 difference))))))))
    merge))

(defun apply-opcode (opcode list1 list2)
  "Apply opcode to list2 and return the result.
opcode is an elements of a differences list."
  (case (difflib:opcode-tag opcode)
    (:delete (delete-sublist list2
			     (difflib:opcode-i1 opcode)
			     (difflib:opcode-i2 opcode)))
    (:replace (replace-sublist list2
			       (subseq list1
				       (difflib:opcode-j1 opcode)
				       (difflib:opcode-j2 opcode))
			       (difflib:opcode-i1 opcode)))
    (:insert (insert-list list2
			  (subseq list1
				  (difflib:opcode-j1 opcode)
				  (difflib:opcode-j2 opcode))
			  (difflib:opcode-i1 opcode)))
    (:equal list2)))

(defun which-differences (differences start end
			  &key (get-start #'difflib:opcode-i1)
			  (get-end #'difflib:opcode-i2))
  "Returns all differences that are about range
start to end. get-start and get-end are used to extract
ranges from each difference."
    (loop for difference in differences
	  when (or (and (<= (funcall get-start difference) start)
			(< start (funcall get-end difference)))
		   (and (> (funcall get-start difference) start)
			(< (funcall get-end difference) end))
		   (and (< (funcall get-start difference) end)
			(<= end (funcall get-end difference))))
	  collect difference
	  until (> (funcall get-start difference) end)))


(defgeneric three-way-merge (ancestor sequence1 sequence2
				      &key delete-markup-function
				      replace-markup-function
				      insert-markup-function
				      equality)
  (:documentation "Perform a three-way-merge.
sequence1 and sequence2 are two independent modifikations of ancestor.
The modifications done in sequence1 are to be merged into sequence2.
This is done by computing the difference between sequence1 and
ancestor and sequence2 and ancestor.
Differences between sequence1 and ancestor are merged into
sequence2 only if sequence2 and ancestor do not differ for this
range."))

(defmethod three-way-merge ((ancestor list) (sequence1 list) (sequence2 list)
			    &key delete-markup-function
			    replace-markup-function
			    insert-markup-function
			    equality)
  "Three-way-merge for lists."
  (let ((differences1 (differences sequence1 ancestor
				   :equality equality))
	(differences2 (differences sequence2 ancestor
				   :equality equality))
	(merge ()))
    (dolist (difference differences1)
      (case (difflib:opcode-tag difference)
	(:delete ())
	(:replace ())
	(:insert ())
	(:equal ())))))