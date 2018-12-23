(in-package :terms)

(defgeneric erase-annotation (term)
  (:documentation "all annotation set to NIL"))

(defmethod erase-annotation ((term term))
  (unless (zeroary term)
    (erase-annotation (root term))
    (mapc #'erase-annotation (arg term))))

(defgeneric remove-annotation (term)
  (:documentation "all annotation set to NIL"))

(defmethod remove-annotation ((term term))
  (if (zeroary term)
      term
      (build-term
       (sym (root term))
       (mapcaro #'remove-annotation (arg term)))))

(defmethod ncompute-annotation-rec ((term term) father fun)
  (unless (zeroary term)
    (setf (annotation (root term)) (funcall fun term father))
    (mapc (lambda (ast)
	    (ncompute-annotation-rec ast term fun))
	  (arg term)))
  term)

(defmethod ncompute-annotation ((term term) fun)
  (ncompute-annotation-rec term nil fun))
 
(defmethod term-to-annotated-term ((term term))
  (build-term
   (symbol-to-annotated-symbol (root term))
   (mapcar #'term-to-annotated-term
	   (arg term))))

(defmethod compute-annotation ((term term) fun)
  (ncompute-annotation (term-to-annotated-term term) fun))
