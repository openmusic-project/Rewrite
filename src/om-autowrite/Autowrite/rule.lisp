(in-package :nautowrite)

(defgeneric left-handside (rule)
  (:documentation "returns the left-handside of RULE"))

(defgeneric right-handside (rule)
  (:documentation "returns the right-handside of RULE"))

(defgeneric left-linear (rule)
  (:documentation "check whether RULE is left-linear"))

(defgeneric growing (rule)
  (:documentation "check whether RULE is growing"))

(defgeneric inverse-rule (rule)
  (:documentation "returns the inverse rule of RULE"))

(defclass rule ()
    ((lh :initarg :lh :accessor left-handside)
     (rh :initarg :rh :accessor right-handside)))

(defun make-rule (lh rh)
  (make-instance 'rule :lh lh :rh rh))

(defmethod print-object ((rule rule) stream)
  (format stream "~A -> ~A"  (left-handside rule) (right-handside rule)))
  
;;; patch a l' avenir il faut separer les regles des transitions
(defmethod compare-object ((rule1 rule) (rule2 rule))
  (and
   (compare-object (left-handside rule1) (left-handside rule2))
   (if (or (casted-state-p (right-handside rule1)) (casted-state-p (right-handside rule2)))
       (eq (right-handside rule1)
	   (right-handside rule2))
       (compare-object (right-handside rule1) (right-handside rule2)))))

(defmethod vars-of ((rule rule))
  (nunion
   (vars-of (left-handside rule))
   (vars-of (right-handside rule)) :test #'compare-object))

(defun arbitrary-rule-p (rule)
  (let ((rh (right-handside rule)))
    (and (var-p rh) (not (my-member rh (vars-of (left-handside rule)))))))

(defun rule-closed-p (rule)
  (my-subsetp (vars-of (right-handside rule))
	      (vars-of (left-handside rule))))

(defmethod left-linear ((rule rule))
  (linear (left-handside rule)))

(defun vars-depth-greater-than-one (lh)
  (and
   (not (var-p lh))
   (let ((vars (vars-of lh)))
     (set-difference
      vars
      (remove-if-not #'var-p (arg lh))))))
   
(defmethod growing ((rule rule))
  (and (left-linear rule)
       (let* ((lh (left-handside rule))
	      (rh (right-handside rule))
	      (vars-to-rename
	       (intersection
		(vars-of rh)
		(vars-depth-greater-than-one lh))))
	 (endp vars-to-rename))))

(defmethod symbols-from ((rule rule))
  (symbol-union
   (symbols-from (left-handside rule))
   (symbols-from (right-handside rule))))

(defun collapsing-rule-p (rule)
  (var-p (right-handside rule)))

(defun standardize-rule (rule)
  (reset-aux-variables)
  (let* ((lh (left-handside rule))
	 (rh (right-handside rule))
	 (vars  (union (vars-of lh) (vars-of rh)))
	 (subst (mapcar #'list
			vars
			(let ((l nil))
			  (dotimes (i (length vars) (nreverse l))
			    (setf l (push (make-aux-var) l)))))))
    (make-rule (apply-substitution lh subst)
	       (apply-substitution rh subst))))

(defgeneric equivalent-rules-p (rule1 rule2)
  (:documentation "equivalence via variables renaming"))

(defmethod equivalent-rules-p ((rule1 rule) (rule2 rule))
  (compare-object (standardize-rule rule1) (standardize-rule rule2)))

(defmethod size ((rule rule))
  (+ (term-size (left-handside rule)) (term-size (right-handside rule))))

(defmethod inverse-rule ((rule rule))
  (make-rule (right-handside rule) (left-handside rule)))

  
