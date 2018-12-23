(in-package :terms)

(defgeneric constructor-symbols-p (term dsymbols)
  (:documentation "checks whether TERM is constructor according to the list of defined symbols DSYMBOLS"))

(defgeneric arg (term)
  (:documentation "list of arguments of TERM"))

(defgeneric root (term)
  (:documentation "root of TERM"))

(defgeneric term-depth (term)
  (:documentation "depth of TERM"))

(defclass abstract-term (signed-object) ())

(defclass term (abstract-term)
  ((root :initarg :root :accessor root)
   (arg :initform nil :initarg :arg :accessor arg)
   (run :accessor run)))

(defun build-term (root &optional (arg nil))
  (make-instance 'term :root root :arg arg))

;;; patch a l' avenir separer les termes des term-states

(defmethod compare-object ((term1 term) (term2 term))
  (and
   (compare-object (root term1) (root term2))
   (= (length (arg term1)) (length (arg term2)))
   (compare-object (arg term1) (arg term2))))

(defun bullet-term ()
  (build-term *bullet-symbol*))

(defun extra-term ()
  (build-term *extra-symbol*))

(defun omega-term ()
  (build-term *omega-symbol*))

(defun term-p (term)
  (typep term 'term))

(defmethod display-sequence ((l sequence) stream &key (sep " "))
  (when l
    (mapc (lambda (e)
	    (format stream "~A~A" e sep))
	  (butlast l))
    (format stream "~A" (car (last l)))))

(defvar *show-run* nil)

(defun toggle-show-run ()
  (setf *show-run* (not *show-run*)))

(defmethod print-object ((term term) stream)
  (let ((root (root term))
	(arg (arg term)))
    (format stream "~A" root)
    (when (and *show-run* (slot-boundp term 'run))
      (format stream "~A" (run term)))
    (unless (zeroary term)
      (format stream "(")
      (display-sequence arg stream :sep ",")
      (format stream ")"))))

(defmethod erase-run ((term term))
  (slot-makunbound term 'run)
  (unless (zeroary term)
    (mapc #'erase-run (arg term))))
 
(defmethod name ((term term))
  (format nil "~A" term))

(defgeneric zerorary (term)
  (:documentation "return TRUE if term is a constant (zeroary symbol, variable or state)"))

(defmethod zeroary ((term term))
  (endp (arg term)))

(defmethod zeroary ((var t)) t)

(defgeneric constant (term)
  (:documentation "return TRUE if term is a constant (zeroary symbol or state)"))

(defmethod constant ((term term))
  (zeroary term))

(defun count-occ-atom (x term)
  (if (zeroary term)
      (if (compare-object x term) 1 0)
      (reduce #'+ (mapcar (lambda (arg) (count-occ-atom x arg)) (arg term)))))

(defmethod term-size ((term abstract-term))
  0)

(defmethod term-size ((term term))
  (if (endp (arg term))
      1
      (1+
       (loop for ar in (arg term)
	     sum (term-size ar)))))

(defmethod term-depth ((term term))
  (if (endp (arg term))
      1
      (1+
       (loop for ar in (arg term)
	     maximize (term-depth ar)))))

(defmethod symbols-from ((term term))
  (if (constant term)
      (list (root term))
      (let ((root (root term))
	    (args (arg term)))
	(symbol-adjoin
	 root
	 (symbol-delete-duplicates
	  (mappend #'symbols-from args))))))

(defmethod signature ((term term))
  (make-signature (symbols-from term)))

(defgeneric my-copy-term (term))

(defmethod my-copy-term ((term term))
  (build-term (root term) (mapcar #'my-copy-term (arg term))))

(defun contains-bullet-p (term)
  (my-member (bullet-term) (subterms term)))

(defmethod constructor-symbols-p ((term term) (dsymbols list))
  (notany (lambda (st) (member (root st) dsymbols))
	  (strict-subterms term)))

(defgeneric build-zeroary-terms-from-symbols (csignature)
  (:documentation "list of zeroary terms in the signature CSIGNATURE containing constant symbols"))

(defmethod build-zeroary-terms-from-symbols ((signature signature))
  (mapcar (lambda (x) (build-term x))
	  (signature-symbols signature)))

(defmethod nodes-satisfying-pred ((term term) pred)
  "Returns a list of the nodes contained in the term satisfying PRED"
   (let ((nodes '()))
     (labels
	 ((intern-get (subterm)
	    (if (funcall pred subterm)
		(push subterm nodes)
		(unless (zeroary subterm)
		  (mapcar #'intern-get (arg subterm))))))
       (intern-get term))
     nodes))

(defmethod leaves-of-term ((term term))
  (nodes-satisfying-pred term #'constant))

(defmethod nb-nodes-satisfying-pred ((term term) pred)
  "Returns a list of the nodes contained in the term satisfying PRED"
   (let ((nb 0))
     (labels
	 ((intern-get (subterm)
	    (if (funcall pred subterm)
		(incf nb)
		(unless (zeroary subterm)
		  (mapcar #'intern-get (arg subterm))))))
       (intern-get term))
     nb))

(defmethod term-nb-leaves ((term term))
  (nb-nodes-satisfying-pred term #'constant))

(defmethod term-randomize-vbits ((term term) (m integer) &optional (exclusive nil))
  (let* ((root (root term))
	 (name (name root)))
    (if (zeroary term)
	(build-term
	 (make-random-vbits-constant-symbol name m exclusive))
	(build-term
	 root
	 (mapcar (lambda (tt)
		   (term-randomize-vbits tt m exclusive))
		 (arg term))))))
