(in-package :nautowrite)

(defgeneric get-terms (sys)
  (:documentation "termset"))

(defgeneric (setf get-terms) (newterms termset)
  (:documentation "replaces the terms in TERMSET by NEWTTERMS"))

(defgeneric  aut-termset (lang)
  (:documentation
   "automaton which recognizes the termset lang"))

(defgeneric termset-merge-signature (termset newsignature)
  (:documentation "merge a signature in a termset signature"))

(defclass termsets ()
  ((termset-table :initform (make-hash-table :test #'equal)
		    :accessor table)))

(defun make-termsets ()
  (make-instance 'termsets))

(defclass termset (named-object)
  ((terms :initform nil
	  :initarg :terms
	  :reader get-terms)
   (aut-termset :initform nil)
   (signature :initform nil
	      :initarg :signature
	      :reader signature)))

(defun make-termset (name terms)
  (make-instance 'termset
		 :name name
		 :terms terms
		 :signature
		 (signature-from terms)))

(defmethod print-object :after ((l termset) stream)
  (format stream ": ")
  (display-sequence (get-terms l) stream)
  (format stream "~%")
  (when (and (not (ground-termset-p (get-terms l))))
    (let ((additional-signature (signature-difference
				 (signature l)
				 (signature-from (get-terms l)))))
      (unless (signature-empty-p  additional-signature)
	(format stream "additional signature: ~A ~%" additional-signature)))))
	

(defmethod (setf get-terms) (newterms (lang termset))
  (with-slots (terms signature aut-termset) lang
    (unless (and
	     (equiv-terms-p terms newterms)
	     (equiv-signature-p (signature-from terms) (signature lang)))
      (setf aut-termset nil)
      (setf signature (signature-from newterms))
      (setf terms newterms)
      )
  terms))

(defmethod aut-termset ((lang termset))
  (with-slots (signature terms aut-termset) lang
    (or aut-termset
	(setf aut-termset
	      (rename-object
	       (nreduce-automaton
		(make-matching-automaton terms signature :finalstates t :subterms nil))
	       (name lang)
	       )))))

(defmethod termset-merge-signature ((lang termset) newsignature)
  (with-slots (signature aut-termset) lang
    (let ((merge-signature (merge-signature signature newsignature)))
      (unless (equiv-signature-p signature merge-signature)
	(setf aut-termset nil signature merge-signature))
      merge-signature)))

(defun ground-termset-p (termset)
  (every #'ground termset))

(defun equiv-termset-p (l1 l2)
  (and
   (equiv-signature-p (signature l1) (signature l2))
   (equiv-terms-p (get-terms l1) (get-terms l2))))

