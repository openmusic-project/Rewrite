(in-package :symbols)

(defvar *signature-mapping*)

(defgeneric signature-from (o)
  (:documentation "signature containing the symbols in o"))

(defmethod signature-from ((o t))
  (make-signature (symbols-from o)))
  
(defgeneric get-arities (signature)
  (:documentation "list of the arities present in the signature without duplicates"))

(defgeneric max-arity (signature)
  (:documentation "maximal arity of the symbols in the signature (0 if signature empty)"))

(defgeneric constant-signature (signature)
  (:documentation "return a signature containing the constant symbols of SIGNATURE"))

(defgeneric non-constant-signature (signature)
  (:documentation "return a signature containing the non constant symbols of SIGNATURE"))

(defgeneric merge-signature (&rest signatures)
  (:documentation "return a signature merging all the SIGNATURES assuming the arities are coherent"))

(defgeneric copy-signature (signature) ; not destructive
  (:documentation "return a signature containing the same symbols as SIGNATURE"))

(defgeneric nb-symbols (signature)
  (:documentation "return the number of symbols in the signature SIGNATURE"))

(defgeneric included-signature-p (signature1 signature2)
  (:documentation "true if SIGNATURE1 is included in SIGNATURE2"))

(defgeneric equiv-signature-p (signature1 signature2)
  (:documentation "true if SIGNATURE1 contains the same symbols that SIGNATURE2"))

(defgeneric adjoin-symbol-to-signature (symbol signature)
  (:documentation "return a new signature containing SYMBOL in addition to the symbols of SIGNATURE"))

(defgeneric remove-symbol-from-signature  (symbol signature)
  (:documentation "return a new signature the symbols of SIGNATURE except SYMBOL"))

(defgeneric remove-symbols-from-signature (symbols signature)
  (:documentation "return a new signature containing the symbols of SIGNATURE
except the ones in the list SYMBOLS"))

(defgeneric add-bullet-symbol (signature)
  (:documentation "return a new signature containing the bullet-symbol in addition to the symbols of SIGNATURE"))

(defgeneric add-extra-symbol (signature)
  (:documentation "return a new signature containing the extra-symbol in addition to the symbols of SIGNATURE"))

(defgeneric remove-bullet-symbol (signature)
  (:documentation "return a new signature containing the symbols of SIGNATURE except the bullet-symbol)"))

(defgeneric remove-extra-symbol (signature)
(:documentation "return a new signature containing the symbols of SIGNATURE except the extra-symbol)"))

(defgeneric signature-intersection (signature1 signature2)
  (:documentation "return a new signature with containing the intersection of the symbols of SIGNATURE1 and SIGNATURE2"))

(defgeneric signature-union (signature1 signature2)
  (:documentation "return a new signature with containing the union of the symbols of SIGNATURE1 and SIGNATURE2"))

(defgeneric signature-difference (signature1 signature2)
  (:documentation "return a new signature with containing the set-difference of the symbols of SIGNATURE1 and SIGNATURE2"))

(defgeneric signature-empty-p (signature)
  (:documentation "check whether a signature is empty"))

(defgeneric signature-symbols (signature)
  (:documentation "list of the symbols of the SIGNATURE"))

(defgeneric signature-arities (signature)
  (:documentation "list of the arities of the symbols in SIGNATURE"))


(defclass signature (signed-object)
  ((symbols :initform nil :initarg :symbols :accessor signature-symbols)))

(defmethod signature ((signature signature))
  signature)

(defmethod print-object ((s signature) stream)
  (let ((*print-arity* t))
    (display-sequence (signature-symbols s) stream))
  s)

(defun make-signature (symbols)
  (setf symbols (remove-duplicates symbols :test #'eq))
  (assert (homogeneous-constant-symbols-p symbols))
  (make-instance 'signature :symbols symbols))

(defmethod get-arities ((signature signature))
  (remove-duplicates (mapcar #'arity (signature-symbols signature)) :test #'=))

(defmethod max-arity ((signature signature))
  (let ((arities (get-arities signature)))
    (if (endp arities)
	0
	(apply #'max arities))))

(defmethod arrange-signature ((signature signature))
  (let ((a (make-array (1+ (max-arity signature)) :initial-element nil)))
    (loop
       for s in (signature-symbols signature)
       do (push s (aref a (arity s))))
    a))

(defmethod constant-signature ((signature signature))
  (make-signature 
   (remove-if-not #'symbol-isconstant (signature-symbols signature))))

(defmethod non-constant-signature ((signature signature))
  (make-signature 
   (remove-if #'symbol-isconstant (signature-symbols signature))))

(defmethod merge-signature (&rest signatures)
  (make-signature
   (symbol-remove-duplicates (apply #'append (mapcar #'signature-symbols signatures)))))

(defmethod copy-signature ((signature signature)) ; not destructive
  (make-signature (copy-list (signature-symbols signature))))

(defmethod nb-symbols ((signature signature))
  (length (signature-symbols signature)))

(defmethod signature-arities ((signature signature))
  (delete-duplicates (mapcar #'arity (signature-symbols signature))))

(defmethod symbols-of-arity ((signature signature) arity)
  (remove-if-not (lambda (sym) (= (arity sym) arity))
		 (signature-symbols signature)))

(defmethod included-signature-p ((s1 signature) (s2 signature))
    (every (lambda (s)
	     (symbol-member s (signature-symbols s2))) (signature-symbols s1)))

(defmethod equiv-signature-p ((s1 signature) (s2 signature))
  (and
   (= (nb-symbols s1) (nb-symbols s2))
   (included-signature-p s1 s2)
   (included-signature-p s2 s1)))

(defmethod adjoin-symbol-to-signature ((sym abstract-arity-symbol) (s signature))
  (make-signature 
    (symbol-adjoin sym (signature-symbols s))))

(defmethod remove-symbol-from-signature  ((sym abstract-arity-symbol) (s signature))
  (make-signature (symbol-remove sym (signature-symbols s))))

(defmethod remove-symbols-from-signature ((syms list) (s signature))
  (make-signature (symbol-set-difference (signature-symbols s) syms)))

(defmethod add-bullet-symbol ((s signature))
  (adjoin-symbol-to-signature *bullet-symbol* s))

(defmethod add-extra-symbol ((s signature))
  (adjoin-symbol-to-signature *extra-symbol* s))

(defmethod add-empty-symbol ((s signature))
  (adjoin-symbol-to-signature *empty-symbol* s))

(defmethod remove-bullet-symbol ((s signature))
  (remove-symbol-from-signature *bullet-symbol* s))

(defmethod remove-extra-symbol ((s signature))
  (remove-symbol-from-signature *extra-symbol* s))

(defmethod signature-intersection ((s1 signature) (s2 signature))
  (make-signature (symbol-intersection (signature-symbols s1) (signature-symbols s2))))

(defmethod signature-union ((s1 signature) (s2 signature))
  (make-signature (symbol-union (signature-symbols s1) (signature-symbols s2))))

(defmethod signature-difference ((s1 signature) (s2 signature))
  (make-signature (symbol-set-difference (signature-symbols s1) (signature-symbols s2))))

(defmethod signature-empty-p ((s signature))
  (endp (signature-symbols s)))

(defun in-signature (sym signature)
  (symbol-member sym (signature-symbols signature)))

(defun in-signature-extra (signature)
  (in-signature *extra-symbol* signature))

(defun in-signature-bullet (signature)
  (in-signature *bullet-symbol* signature))

(defmethod homogeneous-constant-signatures-p ((signature1 signature) (signature2 signature))
   (homogeneous-constant-symbols-p
    (append (signature-symbols (constant-signature signature1))
	    (signature-symbols (constant-signature signature2)))))
