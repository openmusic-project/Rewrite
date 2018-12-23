(in-package :nautowrite)

(defgeneric approximation (rules &key approx)
  (:documentation "approximation of rules according to approx"))

(defun strong-approximation (rule)
  (reset-aux-variables)
  (make-rule (left-handside rule) (make-aux-var)))

(defun nv-rh (rh)
  (rename-vars rh (vars-of rh)))
  
(defun nv-approximation (rule)
  (reset-aux-variables)
  (make-rule (left-handside rule) (nv-rh (right-handside rule))))

(defun growing-rh (rule)
  (let* ((lh (left-handside rule))
	 (rh (right-handside rule))
	 (vars-to-rename
	  (intersection
	   (vars-of rh)
	   (vars-depth-greater-than-one lh)))
	 (subst (mapcar
		 (lambda (vt) (list vt (make-aux-var)))
		 vars-to-rename)))
    (apply-substitution rh subst)))

(defun growing-approximation (rule &key (linear nil))
  (reset-aux-variables)
  (let* ((rh (growing-rh rule))
	 (frh (if linear (linearize-existing rh) rh)))
    (make-rule (left-handside rule) frh)))

(defun linear-growing-approximation (rule)
  (growing-approximation rule :linear t))

(defmethod approximation ((rules list) &key (approx #'growing-approximation))
  (mapcar approx rules))

(defmethod approximation ((rules rules) &key (approx #'growing-approximation))
  (make-rules (approximation (rules-list rules) :approx approx)))

(defun get-approx-name (approx-function-name)
  (case approx-function-name
    (growing-approximation "g")
    (linear-growing-approximation "lg")
    (nv-approximation "nv")
    (strong-approximation "s")))
