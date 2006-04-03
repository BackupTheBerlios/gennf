;; fd.lisp
;;
;; Part of fd, written by Florian Lorenzen <florenz@cs.tu-berlin.de>
;;
;; Contains the solver routines.
;;

(in-package :fd)


;;
;; The reserved symbols of fd constraints.
;;

(defparameter *fd-function* '(fd-add fd-sub fd-mul fd-div fd-mod fd-pow)
  "Arithmetic functions.")
(defparameter *fd-predicate* '(fd-eq fd-neq fd-lt fd-gt fd-le fd-ge)
  "Binary relations.")
(defparameter *fd-domain* '(fd-in)
  "Domain constraint.")
(defparameter *fd-symbols* (append *fd-function* *fd-predicate* *fd-domain*)
  "All together.")
(defparameter *fd-returns* '(fd-delay)
  "Special return values.")


;;
;; The main solver routines: fd-solv, fd-entail, dom-red-solv and
;; their helper functions for local, arc and node consistency.
;;

(defun fd-solv (csp)
  "Return a solution of csp if possible or NIL otherwise. The simple
backtracking solver is used."
  (unless (proper-csp-p csp)
    (error "The csp is malformed."))
  (backtrack-solv csp))

(defun fd-entail (csp constraint)
  "Returns if csp --> constraint for all assignments of all variables
holds. constraint may only be a single constraint.
The entailment is computed by help of fd-solv because fd is negation
closed and a --> b <==> -,a \/ b."
  (let* ((negated-constraint (negation constraint)))
    (cond
      ((not (fd-solv (cons negated-constraint csp))) t)
      ((not (fd-solv (cons constraint csp))) nil)
      (t 'fd-delay))))

(defun dom-red-solv (csp)
  "Returns a solution of csp or NIL if there is none using a
backtracking algorithm with domain reduction."
  (unless (proper-csp-p csp)
    (error "The csp is malformed."))
  ;; Get a node consistent CSP.
  (let ((node-consistent (node-consistency csp)))
    (unless node-consistent
      (return-from dom-red-solv nil))
    ;; Get a local consistent CSP. Remember: local-consistency is
    ;; a generalization of arc-consistency, thus it is not necessary
    ;; to call this separately.
    (let ((local-consistent (local-consistency node-consistent)))
      (unless local-consistent
	(return-from dom-red-solv nil))
      ;; If a local consistent CSP has domains with only one element
      ;; for each variable it is solved and the solution is obvious.
      (when (singleton-domains-p local-consistent)
	(return-from dom-red-solv local-consistent))
      ;; If the CSP is not solved yet, a certain variable is picked
      ;; and its domain halved and a new solution searched.
      ;; As this may fail, the same procedure is retried with the
      ;; initally disregarded domain half.
      ;; Note: this leads to one elemental domains in the worst case
      ;; but it is then possible to check consistency.
      (let* ((variable (choose-variable local-consistent))
	     (domain (find-domain-constraint variable local-consistent))
	     (local-consistent-without-domain
	      (remove-if #'(lambda (constraint)
			     (and (eql (first constraint) 'fd-in)
				  (eql (second constraint) variable)))
			 local-consistent)))
	(multiple-value-bind (first-domain-half second-domain-half)
	    (split-domain-constraint domain)
	  (let ((new-csp (dom-red-solv 
			  (cons first-domain-half
				local-consistent-without-domain))))
	    (if new-csp
		;; The CSP was solved disregarding the second domain half.
		(return-from dom-red-solv new-csp)
		;; The CSP is not solved and it is searched again.
		(let ((new-csp (dom-red-solv
				(cons second-domain-half
				      local-consistent-without-domain))))
		  (if new-csp
		      (return-from dom-red-solv new-csp)
		      (return-from dom-red-solv nil))))))))))

(defun backtrack-solv (csp)
  "Returns a solution of csp or NIL if there is none using backtrack search."
  (unless (proper-csp-p csp)
    (error "The csp is malformed."))
  (let ((node-consistent (node-consistency csp)))
    (unless node-consistent
      (return-from backtrack-solv nil))
    (let ((local-consistent (local-consistency node-consistent)))
      (unless local-consistent
	(return-from backtrack-solv nil))
      (when (singleton-domains-p local-consistent)
	(return-from backtrack-solv local-consistent))
      (let* ((variable (choose-variable local-consistent))
	     (domain (find-domain-constraint variable local-consistent)))
	(dolist (value (third domain))
	  (let ((new-csp (backtrack-solv (cons `(fd-eq ,variable ,value)
					       local-consistent))))
	    (when new-csp
	      (return-from backtrack-solv new-csp))))))))

(defun backtrack-solv-arc-only (csp)
  "Returns a solution of csp or NIL if there is none using backtrack search.
This solving routine only makes the CSP arc-consistent before starting
backtracking. This may have advantages if local-consistency does not
lead to domain reduction."
  (unless (proper-csp-p csp)
    (error "The csp is malformed."))
  (let ((node-consistent (node-consistency csp)))
    (unless node-consistent
      (return-from backtrack-solv-arc-only nil))
    (let ((arc-consistent (arc-consistency node-consistent)))
      (unless arc-consistent
	(return-from backtrack-solv-arc-only nil))
      (when (singleton-domains-p arc-consistent)
	(when (local-consistency arc-consistent)
	  (return-from backtrack-solv-arc-only arc-consistent)))
      (let* ((variable (choose-variable arc-consistent))
	     (domain (find-domain-constraint variable arc-consistent)))
	(dolist (value (third domain))
	  (let ((new-csp (backtrack-solv (cons `(fd-eq ,variable ,value)
					       arc-consistent))))
	    (when new-csp
	      (return-from backtrack-solv-arc-only new-csp))))))))

(defun local-consistency (csp)
  "Return a local consistent csp, if possible, or NIL otherwise."
  (unless (proper-csp-p csp)
    (error "The csp ~A is malformed." csp))
  (multiple-value-bind (domains constraints)
      (split-csp csp)
    (loop with old-domains
	  do (setf old-domains (copy-tree domains))
	  do (loop for constraint in constraints
		   do (setf domains (local-reduce-domains constraint domains))
		   unless domains do (return-from local-consistency nil))
	  until (equal old-domains domains))
    (append domains constraints)))

(defun local-reduce-domains (constraint domains)
  "Reduces the domains of the variables of constraint. constraint must
have more than one variables for this to happen. The routine is a
generalization of arc-reduce-domains and can be used instead."
  (let ((variables (vars constraint)))
    (if (>= (length variables) 2)
	(let ((result-domains (copy-tree domains)))
	  (dolist (variable variables)
	    (labels ((satisfiable (variable value)
		       (exists-solution
			(set-difference variables (list variable))
			(acons variable value ())))
		     (exists-solution (variables assignment)
		       (if (null variables)
			   (fd-satisfied-p constraint assignment)
			   (let ((this-variable (first variables))
				 (rest-variables (rest variables)))
			     (some (lambda (value)
				     (let ((new-assignment
					    (acons this-variable value
						   assignment)))
				       (exists-solution rest-variables
							new-assignment)))
				   (third (find-domain-constraint
					   this-variable result-domains)))))))
	      (let* ((domain (find-domain-constraint variable result-domains))
		     (new-values
		      (remove-if-not #'(lambda (value)
					 (satisfiable variable value))
				     (third domain))))
		(unless new-values
		  (return-from local-reduce-domains nil))
		(setf (third domain) new-values))))
	  result-domains)
	domains)))

(defun arc-consistency (csp)
  "Returns an arc consistent csp or NIL if inconsistent."
  (unless (proper-csp-p csp)
    (error "The csp is malformed."))
  (multiple-value-bind (domains constraints)
      (split-csp csp)
    (loop with old-domains
	  do (setf old-domains (copy-tree domains))
	  do (loop for constraint in constraints
		   do (setf domains (arc-reduce-domains constraint domains))
		   unless domains do (return-from arc-consistency nil))
	  until (equal old-domains domains))
    (append domains constraints)))

(defun arc-reduce-domains (constraint domains)
  "Removes all values from domains that violate the given constraint. The
constraintis only handled if it hat two variables. The function returns
the new domains for all variables or NIL, if the constraint cannot be
fulfilled anymore."
  (let ((variables (vars constraint)))
    (if (= (length variables) 2)
	(let* ((variable1 (first variables))
	       (variable2 (second variables))
	       (result-domains (copy-tree domains))
	       (domain1 (find-domain-constraint variable1 result-domains))
	       (domain2 (find-domain-constraint variable2 result-domains)))
	  (labels ((exists-solution (variable1 value1 variable2 domain2)
		     (let ((assignment (acons variable1 value1 ())))
		       (some #'(lambda (value2)
				 (fd-satisfied-p constraint
						 (acons variable2 value2
							assignment)))
			     domain2))))
	    (let* ((new-values1
		    (remove-if-not
		     #'(lambda (value)
			 (exists-solution variable1 value
					  variable2 (third domain2)))
		     (third domain1)))
		   (new-values2
		    (remove-if-not
		     #'(lambda (value)
			 (exists-solution variable2 value
					  variable1 new-values1))
		     (third domain2))))
	    (unless (and new-values1 new-values2)
	      (return-from arc-reduce-domains nil))
	    (setf (third domain1) new-values1)
	    (setf (third domain2) new-values2)
	    result-domains)))
	domains)))

(defun node-consistency (csp)
  "Returns a node consistent csp or NIL if csp is inconsistent."
  (unless (proper-csp-p csp)
    (error "The csp is malformed."))
  (multiple-value-bind (domains constraints)
      (split-csp csp)
    (dolist (constraint constraints)
      (let ((new-domains (node-reduce-domains constraint domains)))
	(if new-domains
	    (setf domains new-domains)
	    (return-from node-consistency nil))))
    (append domains constraints)))
  
(defun node-reduce-domains (constraint domains)
  "Remove all values from domain-constraints that
violate the given constraint, which must not have more than
one variable. The resulting domains are returned or NIL if the
constraint cannot be fulfilled anymore."
  (let ((variables (vars constraint)))
    (if (= (length variables) 1)
	(let* ((result-domains (copy-tree domains))
	       (domain
		(find-domain-constraint (first variables) result-domains))
	       (new-values
		(remove-if-not
		 #'(lambda (value)
		     (fd-satisfied-p constraint
				     (acons (first variables) value ())))
		 (third domain))))
	  (unless new-values
	    (return-from node-reduce-domains nil))
	  (setf (third domain) new-values)
	  result-domains)
	domains)))


;;
;; Functions to inspect, generate, and transform CSPs.
;; 

(defun vars (constraint)
  "Returns all variables of constraint."
  (cond ((and (symbolp constraint)
	      (not (member constraint *fd-symbols*)))
	 (list constraint))
	((consp constraint)
	 (reduce #'union (mapcar #'vars constraint)))
	(t ())))

(defun interval (variable from to)
  "Generate a domain constraint such that variable is in
the interval from-to by enumeration."
  (let ((enumeration (loop for i from from to to collect i)))
    `(fd-in ,variable ,enumeration)))
  
(defun alldifferent (variables)
  "Generates fd constraints that all variables are pairwise different."
  (reduce #'append
	  (butlast
	   (maplist #'(lambda (variable-list)
			(mapcar #'(lambda (variable)
				    `(fd-neq ,(first variable-list) ,variable))
				(rest variable-list)))
		    variables))))

(defun proper-csp-p (csp)
  "Checks if csp is a proper formed constraint satisfaction problem.
That means for every variable exists one domain constraint and
all other constraints are predicates."
  (let ((variables (vars csp)))
    (dolist (variable variables)
      (when (/= 1 (count-if #'(lambda (constraint)
				(and (eql (first constraint) 'fd-in)
				     (eql (second constraint) variable)))
			    csp))
	(return-from proper-csp-p nil)))
    (dolist (constraint csp)
      (when (not (member (first constraint)
			 (union *fd-predicate* *fd-domain*)))
	(return-from proper-csp-p nil)))
    csp))

(defun singleton-domains-p (csp)
  "Checks, if csp is possibly solved, i. e. the domains of
all variables have exactly one value."
  (let ((domains (split-csp csp)))
    (reduce #'(lambda (a b) (and a b))
	    (mapcar #'(lambda (domain) (= (length (third domain)) 1))
		    domains))))
  
(defun find-domain-constraint (variable csp)
 "Return domain constraint of variable from csp."
 (find-if #'(lambda (constraint)
	      (and (eql (first constraint) 'fd-in)
		   (eql (second constraint) variable)))
	  csp))

(defun choose-variable (csp)
  "Return a variable which has a domain of at least two elements."
  (dolist (domain (split-csp csp))
    (when (>= (length (third domain)) 2)
      (return-from choose-variable (second domain)))))

(defun show-solution (csp)
  "Extract the solution of a solved csp as an alist."
  (if (singleton-domains-p csp)
      (let ((result ()))
	(dolist (domain (split-csp csp))
	  (push (cons (second domain) (first (third domain))) result))
	result)
      (error "csp not solved.")))

(defun solutions-equal (solution1 solution2)
  "Returns if the two solutions are equal. That is, they have the
same variables and these variables have the same values. The solutions
have to be alists as returned by show-solution."
  (let ((translated-symbols-1 (mapcar #'(lambda (assignment)
					  (cons (symbol-name (car assignment))
						(cdr assignment)))
				      solution1))
	(translated-symbols-2 (mapcar #'(lambda (assignment)
					  (cons (symbol-name (car assignment))
						(cdr assignment)))
				      solution2)))
  (and (subsetp translated-symbols-1 translated-symbols-1 :test #'equalp)
       (subsetp translated-symbols-2 translated-symbols-1 :test #'equalp))))

(defun domain-constraint-equal (domain-constraint-1 domain-constraint-2)
  "Returns if the two domain constraints are equal, i. e. if they
restrict the same variables to the same set of numbers."
  (and (equal (second domain-constraint-1) (second domain-constraint-2))
       (subsetp (third domain-constraint-1) (third domain-constraint-2))
       (subsetp (third domain-constraint-2) (third domain-constraint-1))))

(defun domains-equal (csp1 csp2)
  "Returns if csp1 and csp2 have the same domain constraints, i. e. they
have the same variables and those variables are restricted to the same
domains."
  (let ((domains1 (split-csp csp1))
	(domains2 (split-csp csp2)))
    (and (subsetp domains1 domains2 :test #'domain-constraint-equal)
	 (subsetp domains2 domains1 :test #'domain-constraint-equal))))

(defun negation (constraint)
  "Return the negation of constraint which may be any of *fd-predicate*."
  (case (first constraint)
    ('fd-eq (cons 'fd-neq (rest constraint)))
    ('fd-neq (cons 'fd-eq (rest constraint)))
    ('fd-lt (cons 'fd-ge (rest constraint)))
    ('fd-gt (cons 'fd-le (rest constraint)))
    ('fd-le (cons 'fd-gt (rest constraint)))
    ('fd-ge (cons 'fd-lt (rest constraint)))
    (t (error "Constraint ~A is not of ~A." constraint *fd-predicate*))))
	    

(defun split-domain-constraint (domain-constraint)
  "Splits domain-constraint into two domain-constraints, each having
one half of the values."
  (if (< (length (third domain-constraint)) 2)
      (error "Cannot split domain constraint ~A." domain-constraint)
      (values `(fd-in ,(second domain-constraint)
		,(subseq (third domain-constraint) 0
			 (floor (/ (length (third domain-constraint)) 2))))
	      `(fd-in ,(second domain-constraint)
		,(subseq (third domain-constraint)
			 (floor (/ (length (third domain-constraint)) 2)))))))


(defun fd-satisfied-p (constraint assignments)
  "Returns if constraint is satisfied with the given assignment.
If there remain unbound variables it is never satisfied."
  (let ((bound (sublis assignments constraint)))
    (unless (vars bound)
      (if (eql (first bound) 'fd-in)
	  (member (second bound) (third bound))
	  (let ((ground-terms (mapcar #'evaluate (rest bound)))
		(comparator (case (first bound)
			      ('fd-le #'<=)
			      ('fd-ge #'>=)
			      ('fd-lt #'<)
			      ('fd-gt #'>)
			      ('fd-eq #'=)
			      ('fd-neq #'/=))))
	    (apply comparator ground-terms))))))

(defun evaluate (expression)
  "Evaluates ground terms of fd."
  (if (integerp expression)
      expression
      (let ((operator (case (first expression)
			('fd-add #'+)
			('fd-sub #'-)
			('fd-mul #'*)
			('fd-div #'(lambda (a b) (floor (/ a b))))
			('fd-mod #'mod)
			('fd-pow #'expt))))
	(reduce operator (mapcar #'evaluate (rest expression))))))

(defun split-csp (csp)
  "Splits the csp in domain-constraints: fd-in, and
predicate constraints: fd-eq, fd-neq, ..."
  (split csp (lambda (constraint) (eql (first constraint) 'fd-in))))

(defun split (list test-p)
  "All elements of list that fulfill test-p are returned
in the first value, all that do not in the second."
  (if (null list)
      ()
      (multiple-value-bind (fulfill not-fulfill)
	  (split (rest list) test-p)
	(let ((element (first list)))
	  (if (funcall test-p (first list))
	      (values (cons element fulfill) not-fulfill)
	      (values fulfill (cons element not-fulfill)))))))
