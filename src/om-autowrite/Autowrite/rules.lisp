(in-package :nautowrite)

(defvar *dsymbols* nil)

(defclass rules ()
  ((rules-list :initarg :rules-list :reader rules-list)))

(defun make-rules (rules)
  (make-instance
   'rules
   :rules-list
   (remove-duplicates rules :test #'equivalent-rules-p)))

(defmethod print-object ((rules rules) stream)
  (mapc (lambda (rule) (format stream "~A~%" rule))
	(rules-list rules)))

(defun dsymbols (schemes)
  (symbol-remove-duplicates (mapcar #'root schemes)))

(defgeneric right-handsides (rules)
  (:documentation
   "return the set of right-handsides the RULES (without duplicates)"))

(defmethod right-handsides ((rules rules))
  (my-delete-duplicates
   (mapcar #'right-handside (rules-list rules))))

(defgeneric left-handsides (rules)
  (:documentation
   "return the list of the left-handsides of the RULES"))

(defmethod left-handsides ((rules rules))
  (mapcar #'left-handside (rules-list rules)))

(defgeneric rules-closed-p (rules)
  (:documentation "true if each rule of the RULES is closed (contains no variable)"))

(defmethod rules-closed-p ((rules rules))
  (every #'rule-closed-p (rules-list rules)))

(defmethod growing ((rules list))
  (every #'growing rules))

(defmethod growing ((rules rules))
  (growing (rules-list rules)))

(defgeneric collapsing-p (rules)
  (:documentation "true if a rule of RULES is collapsing"))

(defmethod collapsing-p ((rules rules))
  (find-if #'collapsing-rule-p (rules-list rules)))

(defgeneric constructor-p (rules)
  (:documentation "true if all the rules of RULES are constructor"))

(defmethod constructor-p ((rules rules))
  (let* ((lhs (left-handsides rules))
	 (dsymbols (dsymbols lhs))
	 (witness
	  (find-if-not
	   (lambda (st) (constructor-symbols-p st dsymbols))
	   lhs)))
    (values (not witness) witness)))

(defgeneric nb-rules (rules)
  (:documentation "number of rules in RULES"))

(defmethod nb-rules ((rules rules))
  (length (rules-list rules)))

(defgeneric right-linear-rules-p (rules)
  (:documentation "true if every rule in RULES is right-linear"))

(defmethod right-linear-rules-p ((rules rules))
  (every #'linear (right-handsides rules)))

(defmethod left-linear ((rules rules))
  (every #'left-linear (rules-list rules)))

(defgeneric included-rules-p (rules1 rules2)
  (:documentation "true if the rules in RULES1 are included in RULES2 (up to variable renaming)"))

(defmethod included-rules-p ((rules1 rules) (rules2 rules))
  (let ((rules1 (rules-list rules1))
	(rules2 (rules-list rules2)))
    (every (lambda (r)
	     (member r rules2 :test #'equivalent-rules-p))
	   rules1)))

(defgeneric equiv-rules-p (rules1 rules2)
  (:documentation "true if the rules in RULES1 are equivalent to the rules in RULES2 (up to variable renaming)"))

(defmethod equiv-rules-p ((rules1 rules) (rules2 rules))
  (and
   (= (nb-rules rules1) (nb-rules rules2))
   (included-rules-p rules1 rules2)
   (included-rules-p rules1 rules2)))

(defmethod vars-of ((rules rules))
  (vars-of (rules-list rules)))

(defmethod size ((rules rules))
  (reduce #'+ (mapcar #'size (rules-list rules))))

(defgeneric inverse-rules (rules)
  (:documentation "return the inverse of RULES"))

(defmethod inverse-rules ((rules rules))
  (make-rules
   (mapcar #'inverse-rule (rules-list rules))))


(defmethod symbols-from ((rules rules))
  (symbols-from (rules-list rules)))

