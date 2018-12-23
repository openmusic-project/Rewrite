(in-package :nautowrite)

(defgeneric get-lhs (sys)
  (:documentation "left-hand sides"))

(defgeneric get-extra (sys)
  (:documentation "extra symbol in the signature?"))

(defgeneric seqsys-merge-signature (sys newsignature)
  (:documentation "merge the signature (if compatible)"))

(defgeneric seqsys-signature (sys)
  (:documentation "signature"))

(defgeneric seqsys-aut-nf (sys &key bullet extra)
  (:documentation "normal forms automaton"))

(defgeneric seqsys-aut-nf-extra (sys)
  (:documentation "normal forms automaton"))

(defgeneric seqsys-aut-nf-bullet-extra (sys)
  (:documentation "normal forms automaton"))

(defgeneric seqsys-aut-nf-bullet (sys)
  (:documentation "normal forms automaton"))

;; always with bullet?
(defgeneric seqsys-aut-u (sys &key bullet extra)
  (:documentation "aut-u = aut-a union aut-b automaton"))

(defgeneric seqsys-aut-u-t (sys &key bullet)
  (:documentation "aut-u-t = aut-t union aut-b automaton"))

(defgeneric seqsys-aut-t (sys)
  (:documentation "automaton which regognizes T (default aut-nf)"))

(defgeneric seqsys-aut-b (sys &key bullet extra)
  (:documentation "aut-b automaton a toujours bullet"))
;; doit avoir bullet pour aut-d

(defgeneric seqsys-aut-c (sys &key bullet extra deterministic)
  (:documentation "to-normal forms automaton"))

(defgeneric seqsys-aut-c-t (sys &key deterministic simplify)
  (:documentation "to-T automaton"))

(defgeneric seqsys-aut-d (sys &key deterministic partial extra)
  (:documentation "automaton of terms without needed redexes"))

(defgeneric seqsys-aut-d-ndet (sys &key partial extra)
  (:documentation "ndet automaton of terms without needed redexes"))

(defgeneric seqsys-aut-d-det (sys &key partial extra)
  (:documentation "automaton of terms without needed redexes"))

(defgeneric seqsys-aut-d-extra (sys &key deterministic partial)
  (:documentation "automaton of terms without needed redexes"))

(defgeneric seqsys-aut-d-extra-ndet (sys &key partial)
  (:documentation "ndet automaton of terms without needed redexes"))

(defgeneric seqsys-aut-d-extra-det (sys &key partial)
  (:documentation "det automaton of terms without needed redexes"))

(defgeneric get-approx-rules (sys)
  (:documentation "get the approximated rules"))

(defgeneric (setf get-approx-rules) (sys)
  (:documentation "modifies the approximated rules"))

(defclass seqsys (trs)
  ((lhs :initarg :lhs :initform nil :reader get-lhs)
   (signature :initarg :signature :initform (make-signature nil) :reader seqsys-signature)
   (extra :initarg :extra :initform nil :reader get-extra)
   (approx :initform *approximation* :reader seqsys-approx)
   (approx-rules :initarg :approx-rules :initform nil)
   (aut-nf :initform nil)
   (aut-nf-bullet :initform nil)
   (aut-nf-extra :initform nil)
   (aut-nf-bullet-extra :initform nil)
   (aut-u :initform nil)
   (aut-u-extra :initform nil)
   (aut-u-t :initform nil)
   (aut-t :initform nil)
   (aut-c-t :initform nil)
   (aut-c-ndet :initform nil)
   (aut-c-det :initform nil)
   (aut-c-extra-ndet :initform nil)
   (aut-c-extra-det :initform nil)
   (aut-c-bullet-ndet :initform nil)
   (aut-c-bullet-det :initform nil)
   (aut-c-bullet-extra-ndet :initform nil)
   (aut-c-bullet-extra-det :initform nil)
   (aut-b :initform nil)
   (aut-b-extra :initform nil)
   (aut-d-ndet :initform nil)
   (aut-d-det :initform nil)
   (aut-d-extra-ndet :initform nil)
   (aut-d-extra-det :initform nil)
   (aut-d-partial-ndet :initform nil)
   (aut-d-partial-det :initform nil)
   (aut-d-partial-extra-ndet :initform nil)
   (aut-d-partial-extra-det :initform nil)
   ))

(defmethod make-seqsys (name rules &key (extra nil) (signature nil))
  (let* ((default-signature (signature-from rules))
	 (new-signature (merge-signature default-signature signature)))
    (make-instance 'seqsys
		   :name name
		   :rules rules
		   :extra extra
		   :lhs (left-handsides rules)
		   :signature new-signature)))

(defmethod make-seqsys-from-trs ((trs trs))
  (make-seqsys (name trs) (get-rules trs) :signature (signature trs)))

(defmethod (setf get-rules) :before (newrules (sys seqsys))
  (with-slots
	(rules approx-rules signature lhs
	       aut-nf
	       aut-nf-extra
	       aut-nf-bullet-extra
	       aut-nf-bullet
	       aut-u aut-u-extra
	       aut-u-t aut-t
	       aut-c-t
	       aut-c-ndet
	       aut-c-det
	       aut-c-extra-ndet
	       aut-c-extra-det
	       aut-c-bullet-ndet
	       aut-c-bullet-det
	       aut-c-bullet-extra-ndet
	       aut-c-bullet-extra-det
	       aut-b aut-b-extra
	       aut-d-ndet aut-d-det aut-d-extra-ndet aut-d-extra-det) sys
    (let ((newsign (signature-from newrules)))
      (unless (and (equiv-rules-p rules newrules) (equiv-signature-p signature newsign))
	(setf 
	 lhs (left-handsides newrules)
	 signature newsign
	 approx-rules nil
	 aut-nf nil
	 aut-nf-extra nil
	 aut-nf-bullet-extra nil
	 aut-nf-bullet nil
	 aut-u nil
	 aut-u-extra nil
	 aut-u-t nil
	 aut-t nil
	 aut-c-t nil
	 aut-c-ndet nil
	 aut-c-det nil
	 aut-c-extra-ndet nil
	 aut-c-extra-det nil
	 aut-c-bullet-ndet nil
	 aut-c-bullet-det nil
	 aut-c-bullet-extra-ndet nil
	 aut-c-bullet-extra-det nil
	 aut-b nil
	 aut-b-extra nil
	 aut-d-ndet nil
	 aut-d-det nil
	 aut-d-extra-ndet nil
	 aut-d-extra-det nil
	 )))))

(defgeneric trim-automaton-name (char automaton))

(defmethod trim-automaton-name (char (automaton abstract-automaton))
  (setf (name automaton) (remove char (name automaton)))
  automaton)

(defgeneric trim-automaton-name-extra (automaton))
(defmethod trim-automaton-name-extra ((automaton table-automaton))
  (trim-automaton-name #\@ automaton))

(defun trim-automaton-name-bullet (automaton)
  (setf (name automaton) (decompose-name "-o" (name automaton) :from-right t))
  automaton)

(defmethod seqsys-change-approx ((sys seqsys) newapprox)
  (with-slots (approx approx-rules
		      aut-c-ndet
		      aut-c-det
		      aut-c-extra-ndet
		      aut-c-extra-det
		      aut-c-bullet-ndet
		      aut-c-bullet-det
		      aut-c-bullet-extra-ndet
		      aut-c-bullet-extra-det
		      aut-c-t
		      aut-d-ndet aut-d-det aut-d-extra-ndet aut-d-extra-det
		      aut-d-partial-ndet aut-d-partial-det
		      aut-d-partial-extra-ndet aut-d-partial-extra-det
		      ) sys
    (unless (eq approx newapprox)
      (let ((new-approx-rules (approximation
			       (get-rules sys)
			       :approx (symbol-function newapprox))))
	(setf approx newapprox)
	(unless (equiv-rules-p (get-approx-rules sys) new-approx-rules)
	  (setf approx-rules nil
		aut-c-t nil
		aut-c-ndet nil
		aut-c-det nil
		aut-c-extra-ndet nil
		aut-c-extra-det nil
		aut-c-bullet-ndet nil
		aut-c-bullet-det nil
		aut-c-bullet-extra-ndet nil
		aut-c-bullet-extra-det nil
		aut-d-ndet nil
		aut-d-det nil
		aut-d-extra-ndet nil
		aut-d-extra-det nil
		aut-d-partial-ndet nil
		aut-d-partial-det nil
		aut-d-partial-extra-ndet nil
		aut-d-partial-extra-det nil
		approx-rules new-approx-rules))))))

(defun u-slot (extra)
  (if extra
      'aut-u-extra
      'aut-u))

(defmethod seqsys-aut-u ((sys seqsys) &key (bullet nil) (extra nil))
  (let* ((slot-name (u-slot extra))
	 (u-slot (slot-value sys slot-name)))
    (or u-slot
	(let ((aut-u
	       (make-u-automaton
		(signature sys)
		(get-lhs sys)
		(seqsys-aut-nf sys :extra extra)
		:bullet bullet
		:extra extra)))
	  (rename-object aut-u (string-downcase (symbol-name slot-name)))
	  (setf u-slot aut-u)))))

(defmethod seqsys-aut-u-t ((sys seqsys) &key (bullet nil))
  (with-slots (signature rules aut-u-t) sys
    (or aut-u-t
	(setf aut-u-t
	      (make-u-automaton
	       signature
	       (left-handsides rules)
	       (seqsys-aut-t sys)
	       :bullet bullet)))))

(defun nf-slot (bullet extra)
  (if bullet
      (if extra
	  'aut-nf-bullet-extra
	  'aut-nf-bullet)
      (if extra
	  'aut-nf-extra
	  'aut-nf)))

(defmethod seqsys-aut-nf ((sys seqsys) &key (bullet nil) (extra nil))
  (let* ((slot-name (nf-slot bullet extra))
	 (nf-slot (slot-value sys slot-name)))
    (or
     nf-slot
     (let ((aut-nf
	    (make-nf-automaton
	     (get-lhs sys)
	     (signature sys)
	     :bullet bullet
	     :extra extra)))
       (rename-object aut-nf (string-downcase (symbol-name slot-name)))
       (setf (slot-value sys slot-name) aut-nf)))))

(defmethod seqsys-aut-nf-extra ((sys seqsys))
  (seqsys-aut-nf sys :extra t))

(defmethod seqsys-aut-nf-bullet ((sys seqsys))
  (seqsys-aut-nf sys :bullet t))

(defmethod seqsys-aut-nf-bullet-extra ((sys seqsys))
  (seqsys-aut-nf sys :bullet t :extra t))

(defmethod seqsys-aut-t ((sys seqsys))
  (with-slots (lhs signature aut-t aut-nf) sys
    (or aut-t (setf aut-t (seqsys-aut-nf sys)))))

(defmethod seqsys-change-aut-t ((sys seqsys) newaut)
  (with-slots (aut-c-t aut-u-t aut-t signature) sys
    (setf aut-u-t nil
	  aut-c-t nil
	  aut-t newaut)))

(defun b-slot (extra)
  (if extra
      'aut-b-extra
      'aut-b))

(defmethod seqsys-aut-b ((sys seqsys) &key (bullet nil)
			 (extra nil))
  (let* ((slot-name (b-slot extra))
	 (b-slot (slot-value sys slot-name)))
    (or
     b-slot
     (make-b-automaton (get-lhs sys)
		       (signature sys)
		       :extra extra
		       :bullet bullet))))

(defun c-slot (deterministic bullet extra)
  (if deterministic
      (if bullet
	  (if extra
	      'aut-c-bullet-extra-det
	      'aut-c-bullet-det)
	  (if extra
	      'aut-c-extra-det
	      'aut-c-det))
      (if bullet
	  (if extra
	      'aut-c-bullet-extra-ndet
	      'aut-c-bullet-ndet)
	  (if extra
	      'aut-c-extra-ndet
	      'aut-c-ndet))))

;; les aut-c peuvent etre obtenus a partir des aut-c-bullet
;; les aut-c-bullet ne peuvent PAS etre  obtenus a partir 
;; de aut-c-bullet-extra

(defmethod seqsys-aut-c ((sys seqsys)
			 &key (deterministic t) (bullet nil) (extra nil))
  (when (and (not deterministic)
	     (not (right-linear-rules-p (get-approx-rules sys))))
    (format *output-stream*
	    "Jacquemard's algorithm not applicable to non right-linear approximation~%")
    (format *output-stream* "doing Toyama's instead~%")
    (setf deterministic t))
  (let* ((slot-name (c-slot deterministic bullet extra))
	 (c-slot (slot-value sys slot-name)))
    (or c-slot
	(let ((aut-c
	       (if (and (not bullet) (slot-value sys (c-slot deterministic t extra)))
		   (automaton-without-symbol
		    *bullet-symbol* (slot-value sys (c-slot deterministic t extra)))
		   (make-c-automaton
		    (get-approx-rules sys)
		    (seqsys-aut-u sys :extra extra :bullet t)
		    deterministic
		    :simplify t))))
	  (rename-object aut-c (string-downcase (symbol-name slot-name)))
	  (setf (slot-value sys slot-name) aut-c)))))

(defmethod seqsys-aut-c-t ((sys seqsys) &key (deterministic t) (simplify t))
  (with-slots (signature aut-c-t rules) sys
    (or aut-c-t
	(setf aut-c-t
	      (make-c-automaton
	       (get-approx-rules sys)
	       (seqsys-aut-u-t sys)
	       deterministic
	       :simplify simplify)))))

(defun d-slot (deterministic partial extra)
  (if deterministic
      (if partial
	  (if extra
	      'aut-d-partial-extra-det
	      'aut-d-partial-det)
	  (if extra
	      'aut-d-extra-det
	      'aut-d-det))
      (if partial
	  (if extra
	      'aut-d-partial-extra-ndet
	      'aut-d-partial-ndet)
	  (if extra
	      'aut-d-extra-ndet
	      'aut-d-ndet))))

(defmethod seqsys-aut-d ((sys seqsys)
			 &key (deterministic t) (partial nil) (extra nil))
  (let* ((slot-name (d-slot deterministic partial extra))
	 (d-slot (slot-value sys slot-name)))
    (or d-slot
	(let ((aut-d
	       (make-d-automaton
		(seqsys-aut-c sys
			      :deterministic deterministic
			      :bullet t
			      :extra extra)
		(seqsys-aut-b sys :bullet t :extra extra)
		:deterministic deterministic
		:partial partial)))
	  (rename-object aut-d (string-downcase (symbol-name slot-name)))
	  (when (and partial (empty-container-p (get-finalstates aut-d)))
	    ;; in fact not partial
	    (let ((sn (d-slot deterministic nil extra)))
	      (rename-object aut-d (string-downcase (symbol-name sn)))
	      (setf (slot-value sys sn) aut-d)))
	  (setf (slot-value sys slot-name) aut-d)))))

(defmethod seqsys-aut-d-extra-ndet ((sys seqsys) &key (partial nil))
  (seqsys-aut-d sys :deterministic nil  :partial partial :extra t))

(defmethod seqsys-aut-d-extra-det ((sys seqsys) &key (partial nil))
  (seqsys-aut-d sys  :partial partial :extra t))

(defmethod seqsys-aut-d-partial ((sys seqsys) &key (deterministic t)
				 (extra nil))
  (seqsys-aut-d sys :deterministic deterministic :extra extra :partial t))

(defmethod seqsys-aut-d-extra ((sys seqsys) &key (deterministic t) (partial nil))
  (seqsys-aut-d sys :deterministic deterministic :partial partial :extra t))

(defmethod seqsys-aut-d-ndet ((sys seqsys) &key (partial nil) (extra nil))
  (seqsys-aut-d sys :deterministic nil :partial partial :extra extra))

(defmethod seqsys-aut-d-det ((sys seqsys) &key (partial nil) (extra nil))
  (seqsys-aut-d sys :partial partial :extra extra))

(defmethod get-approx-rules ((sys seqsys))
  (with-slots (rules approx-rules approx) sys
    (or approx-rules
	(setf approx-rules (approximation rules :approx (symbol-function approx))))))

(defmethod seqsys-merge-signature ((sys seqsys) newsignature)
  (with-slots (signature aut-nf aut-t aut-u aut-u-extra aut-u-t
			 aut-c-t
			 aut-c-ndet
			 aut-c-det
			 aut-c-extra-ndet
			 aut-c-extra-det
			 aut-c-bullet-ndet
			 aut-c-bullet-det
			 aut-c-bullet-extra-ndet
			 aut-c-bullet-extra-det
			 aut-b aut-b-extra
			 aut-d-det
			 aut-d-ndet) sys
    (let ((merge-signature
	   (merge-signature signature (remove-bullet-symbol newsignature))))
      (unless (equiv-signature-p signature merge-signature)
	(setf aut-nf nil
	      aut-u nil
	      aut-u-extra nil
	      aut-u-t nil
	      aut-t nil
	      aut-c-t nil 
	      aut-c-ndet nil 
	      aut-c-det nil 
	      aut-c-extra-ndet nil 
	      aut-c-extra-det nil 
	      aut-c-bullet-ndet nil 
	      aut-c-bullet-det nil 
	      aut-c-bullet-extra-ndet nil 
	      aut-c-bullet-extra-det nil 
	      aut-d-det nil
	      aut-d-ndet nil
	      signature merge-signature))
      merge-signature)))

(defmethod seqsys-normal-form ((term term) (sys seqsys))
  (recognized-p term (seqsys-aut-nf sys)))

(defmacro with-left-linearity (ctx &rest body)
  `(if (left-linear (get-rules ,ctx))
       ,@body
       (format *output-stream*
	       "method not applicable to non left-linear trss~%")))
