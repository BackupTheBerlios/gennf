;;DESCRIPTION:
;;A pretty simple Testframework from Peter Seibel's book
;;"Practical Programming in Common Lisp".

;;TODO/MISSING:
;;* Condition handeling.
;;* more abstraction
;;** list of function arguments and results 
;;** to be read from a external file
;;* better printing format

(defvar *test-function-name* nil
  "Dynamic/Special variable for printing the actual test-function-name")

(defmacro deftest (name (&rest args) &body body)
  "defines a test function" 
  `(defun ,name ,args
     (let ((*test-function-name* (append *test-function-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "conjuncts and prints the forms"
  `(combine-result
    ,@(loop for f in forms collect
	   `(report-result ,f ',f))))

(defmacro combine-result (&body body)
  "conjuncts its forms"
  (let ((var (gensym)))
    `(let ((,var t))
       ,@(loop for b in body collect 
	      `(if (not ,b) (setf ,var nil)))
       ,var)))

(defun report-result (result form)
  "Printins the form and returning the result"
  (format t "~a: ~a...~:[FAILed~;Passed~]~%" *test-function-name* form result)
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-more-than-on-function2 ()
  (test-more))
(deftest test-more ()
  (combine-result
    (test-something)
    (test-another-thing)))

(deftest test-something () 
  (check
    (= (+ 1 2) 3)
    (= (+ 7 9) 14)
    (= (- 7 7) 0)))

(deftest test-another-thing ()
  (check 
    (= (+ 2 43) 45)))

(test-more-than-on-function2)
