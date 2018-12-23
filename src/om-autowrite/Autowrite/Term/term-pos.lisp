(in-package :terms)

(defgeneric term-paths (term)
  (:documentation "returns the paths of the term TERM"))

(defgeneric term-positions (term)
  (:documentation "return the positions of the term TERM"))

(defgeneric terms-positions (terms)
  (:documentation "return the positions of the terms in TERMS without duplicates"))

(defgeneric term-at-path (term path)
  (:documentation "returns the subterm of TERM at path PATH"))

(defgeneric term-at-position (term position)
  (:documentation "returns the subterm of TERM at position POSITION"))

(defgeneric replace-term-at-path (term path subterm)
  (:documentation "returns a new term like TERM with the subterm at path PATH replaced by SUBTERM"))

(defgeneric replace-term-at-position (term path subterm)
  (:documentation "returns a new term like TERM with the subterm at position POSITION replaced by SUBTERM"))

(defgeneric nreplace-term-at-path (term path subterm)
  (:documentation "replaces subterm at path PATH by SUBTERM"))

(defgeneric replace-terms-at-positions (term positions subterms)
  (:documentation "replaces subterms at positions in POSITIONS by the subterms in SUBTERMS"))

(defmethod term-paths ((term t))
  (declare (ignore term))
  '())
 
(defmethod term-paths ((term term))
  (do ((i 1 (1+ i))
       (argpath (mapcar #'term-paths (arg term)) (cdr argpath))
       (paths nil (append paths
			  (mapcar (lambda (x) (cons i x)) (car argpath)))))
      ((null argpath) (cons nil paths))))

(defmethod term-positions ((term t))
  (mapcar #'make-position (term-paths term)))

(defmethod terms-positions ((terms list))
  (remove-duplicates (mapunion #'term-positions terms) :test #'equal))

(defmethod term-at-path ((term abstract-term) (p list))
  (if (endp p)
      term
      (let ((n (car p))
	    (arg (arg term)))
	(assert (<= n (length arg)))
	(term-at-path (nth (1- n) arg) (cdr p)))))

(defmethod term-at-position ((term term) (p my-position))
  (term-at-path term (path p)))

(defmethod replace-term-at-path ((term abstract-term) (p list) (subterm term))
  (nreplace-term-at-path (my-copy-term term) p subterm))

(defmethod replace-term-at-position
    ((term abstract-term) (p my-position) (subterm abstract-term))
  (replace-term-at-path term (path p) subterm))

;; destructive
(defmethod nreplace-term-at-path ((term abstract-term) (p list) (subterm abstract-term))
  (if (null p)
      subterm
      (let ((i (car p))
	    (arg (arg term)))
	(when (<= i (length arg))
	    (setf (nth (1- i) arg)
		  (replace-term-at-path
		   (nth (1- i) arg)
		   (cdr p)
		   subterm)))
	term)))

(defmethod replace-terms-at-positions ((term term) (positions list) (subterms list))
  (assert (= (length positions) (length subterms)))
  (let ((newterm (my-copy-term term)))
    (mapc (lambda (position subterm)
	    (setf newterm (nreplace-term-at-path newterm (path position) subterm)))
	  positions
	  subterms)
    newterm))

