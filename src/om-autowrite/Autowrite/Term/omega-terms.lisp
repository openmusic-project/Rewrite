(in-package :terms)

(defgeneric omega-term-p (term)
  (:documentation "checks whether TERM is an omega-term"))

(defgeneric omega-positions (term &key non)
  (:documentation "returns the list of omega-positions of TERM (or non-omega-positions if non is T"))

(defgeneric non-omega-positions (term)
  (:documentation "returns the list of non omega-positions of TERM"))

(defgeneric omega-positions-list (terms &key non)
  (:documentation "returns the list of omega-positions of the terms in TERMS (or non-omega-positions if non is T"))

(defgeneric non-omega-positions-list (terms)
  (:documentation "returns the list of omega-positions of the terms in TERMS"))

(defgeneric compatible (term1 term2)
  (:documentation "check whether two terms are compatible"))

(defgeneric le (term1 term2)
  (:documentation "check whether term1 <= term2"))

(defgeneric lt (term1 term2)
  (:documentation "check whether term1 < term2"))

(defgeneric ge (term1 term2)
  (:documentation "check whether term1 >= term2"))

(defgeneric gt (term1 term2)
  (:documentation "check whether term1 > term2"))

(defgeneric compatible-with-a-term (term terms)
  (:documentation "check whether term is compatible with at least a term in TERMS"))

(defgeneric glb (term1 term2)
  (:documentation ""))

(defgeneric lub (term1 term2)
  (:documentation ""))

(defgeneric scheme-from-term (term)
  (:documentation ""))

(defgeneric schemes-from-terms (terms)
  (:documentation ""))

(defgeneric term-from-scheme-rec (term)
  (:documentation ""))

(defgeneric term-from-scheme (term)
  (:documentation ""))

(defgeneric strictly-greater-terms (term terms)
  (:documentation ""))

(defgeneric strictly-smaller-terms (term terms)
  (:documentation ""))

(defgeneric prefixes-of-term (term)
  (:documentation ""))

(defgeneric strict-prefixes-of-term (term)
  (:documentation ""))

(defgeneric strict-prefixes-of-terms (terms)
  (:documentation ""))

(defgeneric extension (term position symbol)
  (:documentation ""))

(defmethod omega-term-p ((term term))
  (compare-object (omega-term) term))

(defmethod omega-positions ((term term) &key (non nil))
  (let ((tp (term-positions term))
	(fn (if non #'remove-if #'remove-if-not)))
    (funcall fn (lambda (p) (omega-term-p (term-at-position term p))) tp)))

(defmethod non-omega-positions ((term term))
  (omega-positions term :non t))

(defmethod omega-positions-list ((terms list) &key (non nil))
  (remove-duplicates
   (mappend (lambda (term) (omega-positions term :non non)) terms)
   :test #'equal))

(defmethod non-omega-positions-list ((terms list))
  (omega-positions-list terms :non t))

(defmethod compatible ((term1 term) (term2 term))
  (or
   (omega-term-p term1)
   (omega-term-p term2)
   (and
    (eq (root term1) (root term2))
    (every #'compatible (arg term1) (arg term2)))))

(defmethod le ((term1 term) (term2 term))
  (or
   (omega-term-p term1)
   (and (eq (root term1) (root term2))
	(every #'le (arg term1) (arg term2)))))

(defmethod lt ((term1 term) (term2 term))
  (and (le term1 term2) (not (compare-object term1 term2))))

(defmethod ge ((term1 term) (term2 term))
  (not (lt term1 term2)))

(defmethod gt ((term1 term) (term2 term))
  (not (le term1 term2)))

(defmethod compatible-with-a-term ((term term) (terms list))
  (find-if (lambda (x) (compatible term x)) terms))
	   
(defmethod glb ((term1 term) (term2 term))
  (if (eq (root term1) (root term2))
      (build-term (root term1) (mapcar #'glb (arg term1) (arg term2)))
      (omega-term)))

(defmethod lub ((term1 term) (term2 term))
  (assert (compatible term1 term2))
  (cond
    ((compare-object (omega-term) term1) (my-copy-term term2))
    ((compare-object (omega-term) term2) (my-copy-term term1))
    ((eq (root term1) (root term2)) (build-term (root term1) (mapcar #'lub (arg term1) (arg term2))))
    (t (error "lub of not compatible terms"))))

(defmethod scheme-from-term ((term var))
  (omega-term))

(defmethod scheme-from-term ((term term))
  (build-term (root term) (mapcar #'scheme-from-term (arg term))))

(defmethod schemes-from-terms ((l list))
  (mapcar #'scheme-from-term l))

(defmethod term-from-scheme-rec ((term abstract-term))
  (if (omega-term-p term)
      (make-aux-var "Z")
      (build-term (root term) (mapcar #'term-from-scheme-rec (arg term)))))

(defmethod term-from-scheme ((term abstract-term))
  (let ((*aux-variables* nil))
    (term-from-scheme-rec term)))

(defmethod strictly-greater-terms ((term abstract-term) (terms list))
  (find-all term terms :test #'lt))

(defmethod strictly-smaller-terms ((term abstract-term) (terms list))
  (find-all term terms :test #'gt))

(defmethod prefixes-of-term ((term abstract-term))
  (my-adjoin
   (omega-term)
   (my-adjoin
    term
    (if (zeroary term)
	'()
	(mapcar (lambda (arg) (build-term (root term) arg))
		(cartesian-product
		 (mapcar #'prefixes-of-term (arg term))))))))

(defmethod strict-prefixes-of-term ((term abstract-term))
  (my-remove term (prefixes-of-term term)))
    
(defmethod strict-prefixes-of-terms ((terms list))
  (my-remove-duplicates (mappend #'strict-prefixes-of-term terms)))

(defmethod extension ((term abstract-term) (p my-position) (sym abstract-arity-symbol))
  (replace-term-at-position term p (build-term sym (make-list (arity sym) :initial-element (omega-term)))))
