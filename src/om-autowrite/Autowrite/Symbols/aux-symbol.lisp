(in-package :symbols)

(defclass aux-constant-symbol (abstract-constant-symbol)
  ((aux-symbol-number :allocation :class :initform 0
		      :accessor aux-symbol-number)
   (aux-constant-symbols :allocation :class :initform nil :accessor symbols)))

(defclass aux-parity-symbol (abstract-parity-symbol)
  ((aux-symbol-number :allocation :class :initform 0
		      :accessor aux-symbol-number)
   (aux-parity-symbols :allocation :class :initform nil
		:accessor aux-symbols)))

(defun reset-aux-parity-symbols (&optional (n 0))
  (let ((s (make-instance 'aux-parity-symbol)))
    (setf (aux-symbol-number s) n)
    (setf (aux-symbols s) nil)))

(defun reset-aux-constant-symbols (&optional (n 0))
  (let ((s (make-instance 'aux-constant-symbol)))
    (setf (aux-symbol-number s) n)
    (setf (aux-symbols s) nil)))

(defun reset-aux-symbols (&optional (n 0))
  (reset-aux-constant-symbols n)
  (reset-aux-parity-symbols n))

(defun make-aux-constant-symbol ()
  (let* ((n (incf (aux-symbol-number (make-instance 'aux-constant-symbol))))
	 (name (if (zerop n)
		   "#c"
		   (format nil "#c~A" n)))
	 (symbol (make-instance 'aux-constant-symbol :name (make-name name))))
    (cast-symbol symbol)))

(defun make-aux-parity-symbol (parity)
  (assert (plusp parity))
  (let* ((n (incf (aux-symbol-number (make-instance 'aux-symbol))))
	 (name (if (zerop n)
		   "#"
		   (format nil "#~A" n)))
	 (symbol (make-instance 'aux-parity-symbol :name (make-name name) :arity parity)))
    (cast-symbol symbol)))

(defun make-aux-symbol (&optional (arity 0))
  (if (zerop arity)
      (make-aux-constant-symbol)
      (make-aux-parity-symbol arity)))

