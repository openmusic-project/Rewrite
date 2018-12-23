(in-package :nautowrite)
(defgeneric make-term-state (term)
  (:documentation "a term associated with an integer"))

(defclass term-state (abstract-state)
  ((term :initarg :term :reader term :initform nil)))

(defmethod make-term-state (term)
  (make-instance 'term-state :term (linearize term)))

(defmethod make-casted-term-state (term)
  (cast-state (make-term-state term)))

(defmethod compare-object ((term-state1 term-state) (term-state2 term-state))
  (or (eq term-state1 term-state2)
      (compare-object (term term-state1) (term term-state2))))

(defmethod print-object ((term-state term-state) stream)
  (format stream "[~A]" (term term-state)))

(defmethod make-term-states (terms)
  (mapcar (lambda (term)
	    (make-term-state term))
	  terms))

(defmethod make-casted-term-states ((terms list))
  (mapcar
   #'cast-state
   (make-term-states terms)))

(defvar *l-name* (make-name "l"))
(defvar *r-name* (make-name "r"))
(defvar *l-symbol* (make-constant-symbol *l-name*))
(defvar *r-symbol* (make-constant-symbol *r-name*))

(defun r-state ()
  (make-term-state (build-term *r-symbol*)))

(defun casted-r-state ()
  (cast-state (r-state)))

(defun l-state ()
  (make-term-state (build-term *l-symbol*)))

(defun casted-l-state ()
  (cast-state (l-state)))

(defun var-state ()
 (make-term-state *new-var*))

(defun casted-var-state ()
  (cast-state (var-state)))

