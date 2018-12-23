(in-package :symbols)
(defvar *commutative-symbols* t)
(defvar *print-unranked* t)

(defclass abstract-symbol (named-object) ()
  (:documentation "abstract class for all symbols"))

(defclass constant-symbol-mixin () ())

(defclass abstract-unranked-symbol (abstract-symbol) ())

(defclass abstract-arity-symbol (abstract-symbol) ()
  (:documentation "abstract class of symbols with arity"))

(defclass abstract-parity-symbol (abstract-arity-symbol parity-mixin) ())

(defclass commutative-symbol-mixin () ())

(defclass abstract-constant-symbol (abstract-arity-symbol constant-symbol-mixin) ())

(defclass constant-symbol (abstract-constant-symbol)
  ((constant-symbols :allocation :class :initform nil :accessor symbols)))

(defclass parity-symbol (abstract-parity-symbol)
  ((parity-symbols :allocation :class :initform nil :accessor symbols)))

(defclass unranked-symbol (abstract-symbol)
  ((unranked-symbols :allocation :class :initform nil :accessor symbols)))

(defclass commutative-unranked-symbol (commutative-symbol-mixin unranked-symbol)
  ((commutative-unranked-symbols :allocation :class :initform nil :accessor symbols)))

(defmethod print-object :after ((s unranked-symbol) stream)
  (when *print-unranked*
    (format stream "%")))

(defmethod sym ((s abstract-arity-symbol)) s)

(defclass empty-symbol (constant-symbol) ())

(defvar *empty-symbol*
  (make-instance 'empty-symbol :name (make-name "@")))

(defclass vbits-constant-symbol (constant-symbol)
  ((vbits-constant-symbols :allocation :class :initform nil :accessor symbols)
   (vbits :initform nil :initarg :vbits :reader vbits)))

(defgeneric symbol-color (abstract-constant-symbol)
  (:documentation "color of CONSTANT-SYMBOL"))

(defclass color-constant-symbol (constant-symbol)
  ((color-constant-symbols :allocation :class :initform nil :accessor symbols)
   (symbol-color :initarg :symbol-color :reader symbol-color)))

(defgeneric vbits-constant-symbol-p (symbol))

(defmethod vbits-constant-symbol-p ((symbol abstract-arity-symbol))
  (typep symbol 'vbits-constant-symbol))

(defmethod color-constant-symbol-p ((symbol abstract-arity-symbol))
  (typep symbol 'color-constant-symbol))

(defvar *predefined-constant-symbols* nil
  "predefined constant symbols omega bullet extra")

(defvar *print-arity* nil)

(defvar *extra-symbol* nil)
(defvar *bullet-symbol* nil)
(defvar *omega-symbol* nil)

(defgeneric symbol-member (symbol l)
  (:documentation "true if SYMBOL is in the list of symbols L"))
(defgeneric symbol-remove (symbol l)
  (:documentation "return a list of the symbols in L except SYMBOL"))
(defgeneric symbol-adjoin (symbol l)
  (:documentation "return a list containing SYMBOL and the symbols"))
(defgeneric symbol-delete (symbol l)
  (:documentation
   "return the list of symbols L where SYMBOL has been deleted (destructive!)"))
(defgeneric symbols-from (o)
  (:documentation "list of the symbols appearing in o"))

(defmethod arity ((s abstract-constant-symbol)) (declare (ignore s)) 0)
(defmethod arity ((s unranked-symbol)) (declare (ignore s)) -1)

(defmethod print-object :after ((s abstract-arity-symbol) stream)
  (when (and *print-arity* (plusp  (arity s)))
    (format stream ":~A" (arity s))))

(defclass commutative-symbol (commutative-symbol-mixin abstract-parity-symbol)
  ((commutative-symbols :allocation :class :initform nil :accessor symbols)))

(defmethod print-object :after ((s commutative-symbol-mixin) stream)
  (format stream "*"))

(defun all-constant-symbols (&key (predefined nil))
  (let ((c (symbols (make-instance 'constant-symbol))))
    (append
     (symbols (make-instance 'color-constant-symbol))
     (if predefined c (set-difference c *predefined-constant-symbols*))
     (symbols (make-instance 'vbits-constant-symbol)))))
 
(defun init-symbols ()
  (setf (symbols (make-instance 'color-constant-symbol)) nil)
  (setf (symbols (make-instance 'constant-symbol)) nil)
  (setf (symbols (make-instance 'vbits-constant-symbol)) nil)
  (setf (symbols (make-instance 'parity-symbol)) nil)
  (setf (symbols (make-instance 'commutative-symbol)) nil)
  (setf (symbols (make-instance 'unranked-symbol)) nil)
  (setf (symbols (make-instance 'commutative-unranked-symbol)) nil)
  (setf *omega-symbol* (make-constant-symbol (make-name "?")))
  (setf *extra-symbol* (make-constant-symbol (make-name "@")))
  (setf *bullet-symbol* (make-constant-symbol (make-name "o")))
  (setf *predefined-constant-symbols* (list *bullet-symbol* *extra-symbol*  *omega-symbol*)))

(defun all-symbols (&key (predefined nil))
  (append
   (all-constant-symbols :predefined predefined)
   (symbols (make-instance 'unranked-symbol))
   (symbols (make-instance 'commutative-unranked-symbol))
   (symbols (make-instance 'parity-symbol))
   (symbols (make-instance 'commutative-symbol))))

(defmethod commutative-symbol-p ((s commutative-symbol))
  t)

(defmethod unranked-symbol-p ((s unranked-symbol))
  t)

(defmethod constant-symbol-p ((s abstract-constant-symbol))
  t)

(defmethod commutative-symbol-p ((s abstract-arity-symbol))
  nil)

(defun arity-symbolp (o)
  (typep 'abstract-arity-symbol o))

(defmethod print-object :after ((s vbits-constant-symbol) stream)
  (when (vbits s)
    (princ #\^ stream)
    (loop for i across (vbits s) do (princ i stream)))
  s)

(defmethod print-object :after ((s color-constant-symbol) stream)
  (princ #\~ stream)
  (princ (symbol-color s) stream))

(defun zero-vbits (m)
  (make-array m :element-type 'bit))

(defgeneric randomize-vbits (bit-vector))

(defmethod randomize-vbits ((vbits bit-vector))
;; destructive
  (let ((m (length vbits)))
    (dotimes (i m vbits)
      (setf (aref vbits i) (random 2)))))

(defun random-vbits (m)
  (randomize-vbits (zero-vbits m)))

(defun random-partition-vbits (m)
  (let ((vbits (zero-vbits m)))
    (setf (aref vbits (random m)) 1)
    vbits))

(defun make-vbits (vbits)
  (when vbits
    (make-array (length vbits) :initial-contents vbits :element-type 'bit)))

(defmethod vbits ((s abstract-symbol)) (declare (ignore s)) nil)

(defun find-symbol-from-name (name &optional (vbits-or-color nil))
  (find-if
   (lambda (s)
     (when (eq (name s) (make-name name))
       (or (not vbits-or-color)
	   (cond ((color-constant-symbol-p s)
		  (= (symbol-color s) vbits-or-color))
		 ((vbits-constant-symbol-p s)
		  (equal (vbits s) (make-vbits vbits-or-color)))
		 (t nil)))))
   (all-symbols :predefined t)))

(defmethod make-vbits-constant-symbol (name (vbits list))
  (if vbits
      (make-vbits-constant-symbol name (make-vbits vbits))
      (make-constant-symbol name)))

(defmethod cast-symbol ((symbol abstract-symbol))
  (or (find-object symbol (symbols symbol))
      (car (push symbol (symbols symbol)))))
  
(defmethod make-vbits-constant-symbol (name (vbits bit-vector))
  (let ((symbol (make-instance 'vbits-constant-symbol :name (make-name name) :vbits vbits)))
    (cast-symbol symbol)))

(defmethod make-random-vbits-constant-symbol (name m &optional (exclusive nil))
  (make-vbits-constant-symbol
   name
   (if exclusive
       (random-partition-vbits m)
       (random-vbits m))))

(defmethod make-random-partition-vbits-constant-symbol (name m)
  (make-random-vbits-constant-symbol name m t))

(defmethod make-zero-vbits-constant-symbol (name m)
  (make-vbits-constant-symbol name (zero-vbits m)))

(defun make-color-constant-symbol (name symbol-color)
  (let ((symbol
	 (make-instance
	  'color-constant-symbol
	  :name (make-name name)
	  :symbol-color symbol-color)))
    (cast-symbol symbol)))

(defmethod make-random-color-constant-symbol (name k)
  (make-color-constant-symbol name (1+ (random k))))

(defun make-constant-symbol (name)
  (let ((symbol (make-instance 'constant-symbol :name (make-name name))))
    (cast-symbol symbol)))

(defmethod compare-object :around ((s1 abstract-symbol) (s2 abstract-symbol))
  (and (eq (type-of s1) (type-of s2))
       (call-next-method)))

(defmethod compare-object
    ((s1 vbits-constant-symbol) (s2 vbits-constant-symbol))
  (and (call-next-method) ;; to compare names
       (equal (vbits s1) (vbits s2))))

(defmethod compare-object
    ((s1 color-constant-symbol) (s2 color-constant-symbol))
  (and (call-next-method) ;; to compare names
       (= (symbol-color s1) (symbol-color s2))))

 (defmethod compare-object ((s1 parity-symbol) (s2 parity-symbol))
;;   (format (error-output* "compare-object parity parity ~A ~A~%" s1 s2)
   (and (call-next-method) (= (arity s1) (arity s2))))

(defmethod compare-object ((s1 commutative-symbol) (s2 commutative-symbol))
  (and (= (arity s1) (arity s2))
       (eq (name s1) (name s2))))

;; are the two following methods necessary?
;; we shoud not have a parity-symbol and a commutative symbol with the same name?
(defmethod compare-object ((s1 parity-symbol) (s2 commutative-symbol))
  (declare (ignore s1 s2))
  nil)
(defmethod compare-object ((s1 commutative-symbol) (s2 parity-symbol))
  (declare (ignore s1 s2))
  nil)

(defun make-parity-symbol (name arity)
  (let ((symbol (make-instance 'parity-symbol :name (make-name name) :arity arity)))
    (cast-symbol symbol)))

(defun make-unranked-symbol (name &optional (commutative nil))
  (let ((symbol
	 (make-instance
	  (if commutative
	      'commutative-unranked-symbol
	      'unranked-symbol) :name (make-name name))))
    (cast-symbol symbol)))

;; non commutative symbol with arity
(defun make-arity-symbol (name arity)
  (if (zerop arity)
      (make-constant-symbol name)
      (make-parity-symbol name arity)))

(defun make-commutative-symbol (name arity)
  (let ((symbol (make-instance 'commutative-symbol :name (make-name name) :arity arity)))
    (cast-symbol symbol)))

(defun make-commutative-bin-symbol (name)
  (make-commutative-symbol name 2))

(defun symbol-isconstant (symbol)
  (zerop (arity symbol)))

(defmethod symbol-member ((s abstract-symbol) (l list))
  (member s l :test #'eq))

(defmethod symbol-remove ((s abstract-symbol) l)
  (remove s l :test #'eq))

(defun symbol-subsetp (l1 l2)
  (subsetp l1 l2 :test #'eq))

(defmethod symbol-adjoin ((s abstract-symbol) l)
  (adjoin s l :test #'eq))

(defmethod symbol-delete ((s abstract-symbol) l)
  (delete s l :test #'eq))

(defun symbol-intersection (l1 l2)
  (intersection l1 l2 :test #'eq))

(defun symbol-union (l1 l2)
  (union l1 l2 :test #'eq))

(defun symbol-remove-duplicates (l)
  (remove-duplicates l :test #'eq))

(defun symbol-delete-duplicates (l)
  (delete-duplicates l :test #'eq))

(defun symbol-set-difference (l1 l2)
  (set-difference l1 l2 :test #'eq))

(defmethod symbols-from ((l list))
  (symbol-delete-duplicates (mappend #'symbols-from l)))

(defmethod constant-symbols (symbols)
  (remove-if-not #'symbol-isconstant symbols))

(defun homogeneous-constant-symbols-p (symbols)
  (let ((constants (constant-symbols symbols)))
    (or (null (cdr constants))
	(null (cdr
	       (remove-duplicates
		constants
		:test #'= :key (lambda (s) (length (vbits s)))))))))

  