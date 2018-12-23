(in-package :nautowrite)
(defvar *initial-index-point* nil)
(defvar *index-point-number* 0)

;;; A position associated with a list of pointers to such position in a list of prefixes
(defclass occurrence () ())

(defclass especial-occurrence (occurrence)
  ((especial-tag :initform nil :initarg :especial-tag :reader especial-tag)))

(defmethod print-object ((u especial-occurrence) stream)
  (format stream "~A" (especial-tag u)))

(defclass normal-occurrence ()
  ((prefixes :initform nil :initarg :prefixes :reader prefixes)
   (pos :initform nil :initarg :pos :reader pos)))

(defclass omega ()
  ((occurrence :initform nil :initarg :occurrence :reader occurrence)
   (possible :initform t :accessor possible)))

(defgeneric make-index-point (prefix omegas schemes subschemes
				     &optional parent incoming-sym)
  (:documentation "create an index-point in an index-tree"))
  
(defgeneric delta (sym s)
  (:documentation "transition function for an index tree: gives an index-point given index-point S and symbol SYM"))

(defgeneric initial-index-point-p (s)
  (:documentation "true is S is the initial index-point of an index-tree"))

(defgeneric any-index (s)
  (:documentation "extract the first possible index occurrence in index-point S"))

(defgeneric transition-symbols (s)
  (:documentation "return a list of all transitions symbols labelling transitions issued from index-point S"))
 
(defgeneric given-index (s)
  (:documentation "extracts the given index occurrence in index-point S"))

(defgeneric eject (ss u sym)
  (:documentation "removes from the list of subschemes SS the ones that do not have symbol SYM at position U"))

(defgeneric extend-omegas (omegas u sym schemes subschemes)
  (:documentation "modification of the list of omega-occurrences corresponding to the extension of the prefix by symbol SYM at position U"))

(defgeneric compute-failure-transition (s)
  (:documentation "compute failure index-point of index-point S"))

(defgeneric forward (s)
  (:documentation "compute the states accessible from S via transfer (delta) transitions"))

(defgeneric forward-branching-index-tree (sys)
  (:documentation "compute the forward-branching index tree of a system or return nil if system not forward-branching"))

(defmethod print-object ((u normal-occurrence) stream)
  (show (pos u) stream))

(defmethod print-object ((o omega) stream)
  (format stream "~A" (occurrence o))
  (when (not (possible o))
    (format stream "!")))

(defun make-occurrence (pos prefixes)
  (make-instance 'normal-occurrence :pos pos :prefixes prefixes))

(defun make-omega (occurrence)
  (make-instance 'omega :occurrence occurrence))

(defun make-especial-occurrence (tag)
  (make-instance 'especial-occurrence :especial-tag tag))

(defclass index-point (abstract-state)
  ((ip-number :initform (incf *index-point-number*) :reader ip-number)
   (index :initform (make-especial-occurrence 'unknown) :accessor index)
   (delta-alist :initform '() :accessor delta-alist)
   (phi :initform '() :accessor phi)
   (prefix :accessor prefix :initarg :prefix)
   (omegas :initform '() :accessor omegas :initarg :omegas)
   (match :initform -1 :accessor match)
   (schemes :initform '() :accessor schemes :initarg :schemes)
   (subschemes :initform '() :accessor subschemes :initarg :subschemes)
   (parent :initform nil :accessor parent :initarg :parent)
   (incoming-sym :initform nil :accessor incoming-sym :initarg :incoming-sym)
   ))

(defmethod make-index-point ((prefix term) (omegas list) (schemes list) (subschemes list)
			     &optional (parent nil) (incoming-sym nil))
  (make-instance 'index-point
		 :prefix prefix
		 :omegas omegas
		 :schemes schemes
		 :subschemes subschemes
		 :parent parent
		 :incoming-sym incoming-sym))

(defmethod print-object ((s index-point) stream)
  (format stream "s~A = [~A,~A] " (ip-number s) (prefix s) (index s)))

(defmethod show ((s index-point) &optional (stream t))
  (format stream "~A " s)
  (unless (or (null (phi s)) (null (phi (phi s))))
    (format stream " phi(s~A) = s~A" (ip-number s) (ip-number (phi s))))
  (unless (endp (delta-alist s))
    (format stream " ~A" (delta-alist s)))
  (format stream "~%")
  (dolist (p (delta-alist s))
    (show (cdr p) stream)))

(defun trivial-index-point (schemes subschemes)
  (let* ((ns (length schemes))
	 (prefixes (append schemes subschemes))
	 (n (+ ns (length subschemes))))
  (make-index-point
   (omega-term)
   (list (make-omega (make-occurrence (make-position '()) (make-array n :initial-contents prefixes))))
   (suite 0 (1- ns))
   (suite ns (1- n)))))

(defmethod delta ((sym abstract-arity-symbol) (s index-point))
  (cdr (assoc sym (delta-alist s) :test #'eq)))

(defmethod initial-index-point-p ((s index-point))
  (eq s *initial-index-point*))

(defmethod initial-index-point-p ((s (eql nil)))
  t)

(defmethod phi ((s (eql nil)))
  nil)

(defun check-directions (u ss)
  (notany (lambda (s) (omega-term-p (aref (prefixes u) s)))
	  ss))

(defun match-scheme (omegas scheme-number)
  (every (lambda (o)
	   (omega-term-p (aref (prefixes (occurrence o)) scheme-number)))
	 omegas))

(defgeneric possible-positions (omegas schemes-numbers))

(defmethod possible-positions ((omegas list) (numbers list))
  (let ((nbposs 0))
    (dolist (o omegas)
      (when (possible o)
	(if (check-directions (occurrence o) numbers)
	    (incf nbposs)
	    (setf (possible o) nil))))
      nbposs))

(defmethod any-index ((s index-point))
  (occurrence (find-if #'possible (omegas s))))

(defmethod given-index ((s index-point))
  (let* ((fs (phi s))
	 (u (index fs))
	 (os (omegas s)))
    (dolist (o (omegas fs))
      (if (eq (occurrence o) u)
	  (return (occurrence (car os)))
	  (setf os (cdr os))))))

(defmethod transition-symbols ((s index-point))
  (delete-duplicates (mapcar
		      (lambda (scheme) (root (aref (prefixes (index s)) scheme)))
		      (schemes s))
		     :test #'eq))

(defmethod eject ((ss list) (u normal-occurrence) (sym abstract-arity-symbol))
  (remove-if-not (lambda (s) (eq sym (root (aref (prefixes u) s))))
		 ss))

(defun duplicate-omegas-except-index (omegas u)
  (let ((new '()))
    (dolist (o omegas (nreverse new))
      (unless (eq (occurrence o) u)
	(push (make-omega (occurrence o)) new)))))

(defmethod extend-omegas ((omegas list) (u normal-occurrence) (sym abstract-arity-symbol) schemes subschemes)
  (append (mapcar (lambda (i)
		    (let* ((newp (extend-position (pos u) i))
			   (newprefixes (make-array (length (prefixes u)))))
		      (dolist (j (append schemes subschemes))
			(setf (aref newprefixes j) (nth (1- i) (arg (aref (prefixes u) j)))))
		      (make-omega (make-occurrence newp newprefixes))))
		  (suite 1 (arity sym)))
	  (duplicate-omegas-except-index omegas u)))

;; the initial index point will be such that phi(s) = nil
(defmethod compute-failure-transition ((s index-point))
  (unless (initial-index-point-p s)
    (setf (phi s) *initial-index-point*)
    (do ((fp (phi (parent s)) (phi fp)))
	((null fp))
      (let ((ts (delta (incoming-sym s) fp)))
	(when ts
	  (setf (phi s) ts)
	  (return)))))
  (phi s))

(defmethod forward ((s index-point))
  (let ((u (index s))
	(syms (transition-symbols s)))
    (mapcar
     (lambda (sym)
       (let* ((prefix (extension (prefix s) (pos u) sym))
	      (schemes (eject (schemes s) u sym))
	      (subschemes (eject (subschemes s) u sym))
	      (omegas (extend-omegas (omegas s) u sym schemes subschemes))
	      (news (make-index-point prefix omegas schemes subschemes s sym)))
	 (push (cons sym news) (delta-alist s))
	 news))
     syms)))

(defun retrieve-scheme (j)
  (aref (prefixes (index *initial-index-point*)) j))

(defmethod forward-branching-index-tree ((sys trs))
  (forward-branching-index-tree (schemes-from-terms (left-handsides (get-rules sys)))))

;;; check fb by constructing a fb index tree
;;; quadratic complexity
(defmethod forward-branching-index-tree ((schemes list))
  (handler-case
      (let* ((*dsymbols* (dsymbols schemes))
	     (subschemes (remove-if-not #'subscheme-p (strict-subterms-list schemes)))
	     (*index-point-number* -1)
	     (*initial-index-point* (trivial-index-point schemes subschemes))
	     (todo  (list *initial-index-point*)))
	(do
	 ()
	 ((endp todo) *initial-index-point*)
	  (let ((s (pop todo)))
	    (dolist (j (schemes s))
	      (when (match-scheme (omegas s) j)
		(setf (match s) j)
		(setf (index s) (make-especial-occurrence 'final))
		(return)))
	    (when (>= (match s) 0)
	      (when (> (length (schemes s)) 1)
		(return (values nil (format nil "overlapp between scheme ~A and scheme ~A"
					    (retrieve-scheme (match s))
					    (retrieve-scheme (if (= (match s) (car (schemes s))) (cadr (schemes s)) (car (schemes s))))
					    ))))
	      (when (> (length (subschemes s)) 0)
		(return-from forward-branching-index-tree
		  (values nil
			  (format nil "overlapp between scheme ~A and subscheme ~A"
				  (retrieve-scheme (match s))
				  (retrieve-scheme (car (subschemes s))))))))
	    (unless (>= (match s) 0)
		  (compute-failure-transition s)
		  (when (zerop  (possible-positions (omegas s) (schemes s)))
		    (return
		      (values nil (format nil "and not strongly sequential witnessed by ~A" (prefix s)))))
		  (when
		      (and (initial-index-point-p (phi s))
			   (zerop (possible-positions (omegas s) (subschemes s))))
		    (return (values nil (format nil "witnessed by ~A" (prefix s)))))
		  (setf (index s)
			(if (initial-index-point-p (phi s))
			    (any-index s)
			    (given-index s)))
		  (setf todo
			(append todo (forward s)))))))))
