(in-package :terms)

(defmethod my-copy-term ((term var))
  term)

(defmethod arg ((var var))
  nil)

(defmethod constant ((term var))
  nil)

(defvar *new-var*
  (make-aux-var)
  "internal variable term not belonging to the defined variable terms")

(defmethod vars-of ((term term))
  (vars-of (arg term)))

(defun linearize-arg (args)
  (if (endp args)
      args
      (cons
       (linearize-rec (car args))
       (linearize-arg (cdr args)))))

(defgeneric linearize-rec (term))

(defmethod linearize-rec ((term var))
  (make-aux-var))

(defmethod linearize-rec ((term term))
  (build-term (root term) (linearize-arg (arg term))))

(defun linearize (term)
  (reset-aux-variables)
  (linearize-rec term))

(defun linear (term)
  (every
   (lambda (x) (= 1 x))
   (mapcar
    (lambda (v) (count-occ-atom v term))
    (vars-of term))))

(defun linear-terms-p (terms) (every #'linear terms))

(defun ground (term)
  (zerop (count-vars-of term)))

(defun subterms (term &key (var nil)); if var returns also variable subterms
  (cond
    ((var-p term) 
     (if var (list *new-var*) '()))
    ((zeroary term)
     (list term))
    (t 
     (my-adjoin (linearize term) (subterms-list (arg term) :var var)))))

(defun strict-subterms (term &key (var nil))
  (if (or (var-p term) (zeroary term))
      '()
      (subterms-list (arg term) :var var)))

(defun subterms-list (prog &key (var nil))
  (my-delete-duplicates (mapcan (lambda (x) (subterms x :var var)) prog)))
 
(defun strict-subterms-list (prog &key (var nil))
  (my-delete-duplicates (mapcan (lambda (x) (strict-subterms x :var var)) prog)))

(defun count-vars-of (term)
  (length (vars-of term)))

(defun rename-vars-arg (args vars)
  (if (null args)
      nil
      (cons
       (rename-vars (car args) vars)
       (rename-vars-arg
        (cdr args)
        vars))))

(defun rename-vars (term vars)
  (if (and (var-p term) (my-member term vars))
      (make-aux-var)
      (build-term (root term) (rename-vars-arg (arg term) vars))))

(defun linearize-existing (term)
  (let ((vars (vars-of term)))
    (labels ((rename-existing-vars (term)
	       (if (var-p term)
		   (if (member term vars)
		       (progn
			 (setf vars (delete term vars))
			 term)
			 (make-aux-var))
		   (build-term
		    (root term)
		    (mapcar #'rename-existing-vars (arg term))))))
      (rename-existing-vars term))))

(defmethod symbols-from ((term var))
  '())

(defgeneric bullet-bindings (vars)
  (:documentation "returns a list of bindings where every variable of VARS is bounded to bullet"))

(defgeneric substitute-bullet (term)
  (:documentation "returns TERM to which a bullet substitution has been applied"))

(defmethod bullet-bindings ((vars list))
  (mapcar
   (lambda (v) (make-binding v (bullet-term)))
   vars))

(defmethod substitute-bullet ((term term))
  (let* ((vars (vars-of term))
	 (substi (bullet-bindings vars)))
    (apply-substitution term substi)))
 
(defgeneric apply-substitution (term substitution))

(defmethod apply-substitution ((term t) substitution)
;;  (format *error-output* "Warning apply-substitution ~A~%" (type-of term))
  term)

(defmethod apply-substitution ((term var) substitution)
  (let ((pair (my-assoc term substitution)))
    (if (null pair) term (cadr pair))))

(defmethod apply-substitution ((term term) substitution)
  (build-term
   (root term)
   (mapcar
    (lambda (x) (apply-substitution x substitution))
    (arg term))))
