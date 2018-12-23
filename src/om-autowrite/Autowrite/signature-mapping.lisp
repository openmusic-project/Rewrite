(in-package :nautowrite)

(defgeneric apply-signature-mapping (object))

(defmethod apply-signature-mapping :around ((object signed-object))
  (if (endp *signature-mapping*)
      object
      (call-next-method)))

(defmethod injective-mapping-p ((mapping list))
  (let ((newsymbols (mapcar #'cadr mapping)))
    (= (length newsymbols)
       (length (remove-duplicates newsymbols :test #'eq)))))

(defmethod apply-signature-mapping ((state abstract-state))
  state)

(defmethod apply-signature-mapping ((l list))
  (mapcar
   (lambda (object) (apply-signature-mapping object))
   l))

(defmethod apply-signature-mapping ((term term))
  (let ((root (apply-signature-mapping (root term))))
    (build-term (if (listp root)
		    (car root)
		    root)
		(apply-signature-mapping (arg term)))))

(defmethod apply-signature-mapping ((signature signature))
  (make-signature
   (let ((newsymbols ()))
     (dolist (symbol (signature-symbols signature) newsymbols)
       (setf newsymbols
	     (append
	      (apply-signature-mapping symbol)
	      newsymbols))))))

(defmethod apply-signature-mapping ((s abstract-arity-symbol))
  (cdr (assoc s *signature-mapping*)))

(defmethod apply-signature-mapping ((s annotated-parity-symbol))
  (mapcar
   (lambda (sym)
     (make-annotated-parity-symbol sym (annotation s)))
   (apply-signature-mapping (sym s))))
;;  (warn "apply-signature-mapping to annotated-symbol"))

(defmethod napply-signature-mapping ((automaton table-automaton))
  (setf (signature automaton)
	(apply-signature-mapping (signature automaton)))
  (napply-signature-mapping (transitions-of automaton))
  (unset-equivalence-classes automaton)
  (set-complete-unknown automaton)
  automaton)

(defmethod apply-signature-mapping ((automaton table-automaton))
  (napply-signature-mapping (duplicate-automaton automaton)))

(defun inverse-mapping (mapping &key (test #'eq))
  (let ((bmapping '()))
    (loop
      for l in mapping
      do (loop
	   for s in (cdr l)
	   do (let ((ll (find s bmapping :key #'car :test test)))
		(if ll
		    (push (car l) (cdr ll))
		    (push (list s (car l)) bmapping)))))
    bmapping))
