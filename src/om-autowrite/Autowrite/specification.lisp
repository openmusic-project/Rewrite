(in-package :nautowrite)

(defvar *loadable* nil)

(defvar *spec* nil "current specification")
(defvar *specs* nil "hash-table of loaded specifications")

(defclass specs ()
   ((specs-table :initform (make-hash-table :test #'equal)
 	  :accessor table)))

(defun make-specs ()
  (make-instance 'specs))

(defclass spec (named-object)
  ((signature :initform nil :initarg :signature :accessor signature)
   (vars :initform nil :initarg :vars :accessor vars)
   (trs :initform nil :initarg :trs :accessor trs)
   (automaton :initform nil :initarg :automaton :accessor automaton)
   (tautomaton :initform nil :initarg :tautomaton :accessor tautomaton)
   (termset :initform nil :initarg termset :accessor termset)
   (trss :initform (make-trss) :accessor trss)
   (automata :initform (make-automata) :accessor automata)
   (tautomata :initform (make-tautomata) :accessor tautomata)
   (termsets :initform  (make-termsets) :accessor termsets)
   (terms :initform  (make-terms) :accessor terms)
   (term :initform nil :initarg :term :accessor term)
   (previous-term :initform nil :initarg :previous-term :accessor previous-term)
   ))

(defgeneric add-automaton (a spec))

(defmethod add-automaton ((a abstract-automaton) spec)  
  (setf (gethash (name a) (table (automata spec))) a))

(defgeneric remove-automaton (a spec))

(defmethod remove-automaton ((a abstract-automaton) spec)  
  (remhash (name a) (table (automata spec))))

(defun remove-tautomaton (a spec)  
  (remhash (name a) (table (tautomata spec))))

(defun add-current-automaton ()
  (add-automaton (automaton *spec*) *spec*))

(defun add-tautomaton (a spec)  
  (setf (gethash (name a) (table (tautomata spec))) a))

(defun add-current-tautomaton ()
  (add-tautomaton (tautomaton *spec*) *spec*))

(defun add-term (term spec)
  (setf (gethash (name term) (table (terms spec))) term))

(defun add-current-term ()
  (add-term (term *spec*) *spec*))

(defun add-termset (l spec)
  (setf (gethash l (table (termsets spec))) l))

(defun add-current-termset ()
  (add-termset (termset *spec*) *spec*))

(defun add-current-trs ()
  (add-trs (trs *spec*) *spec*))

(defun add-trs (trs spec)
  (setf (gethash (name trs) (table (trss spec))) trs))

(defun fill-spec (spec &key (trss nil) (termsets nil) (automata nil) (tautomata nil) (terms nil))
  (loop for trs in trss
     do (add-trs trs spec))
  (loop for termset in termsets
     do (add-termset termset spec))
  (loop for automaton in automata
     do (add-automaton automaton spec))
  (loop for tautomaton in tautomata
     do (add-tautomaton tautomaton spec))
  (loop for term in terms
     do (add-term term spec))
  spec)
  
(defun make-spec
    (name signature vars &key (trss nil) (termsets nil) (automata nil) (tautomata nil) (terms nil))
  (let ((spec (make-instance 'spec
		 :name name
		 :signature signature
		 :vars vars
		 :automaton (car automata)
		 :tautomaton (car tautomata))))
    (fill-spec spec :trss trss :termsets termsets :automata automata :tautomata tautomata :terms terms)))

(defun load-spec (stream)
  (handler-case
      (parse-spec stream)
    (autowrite-parse-error (c)
      (prog1 nil (format *error-output* "load-spec parse error ~A: ~A~%" (token c) (message c))))
    (all-errors ()
      (prog1 nil (format *error-output* "error~%")))))

(defun read-spec-from-path (path)
  (let ((spec (load-file-from-absolute-filename path #'load-spec)))
    (when spec
      (build-spec (file-namestring path) spec))))

(defun load-trs-spec (stream)
  (handler-case
      (parse-trs-spec stream)
    (autowrite-parse-error (c)
      (prog1 nil (format *error-output* "load-trs-spec parse error ~A: ~A~%" (token c) (message c))))
    (all-errors ()
      (prog1 nil (format *error-output* "error~%")))))

(defun load-aut-spec (stream)
  (handler-case
      (parse-aut-spec stream)
    (autowrite-parse-error (c)
      (prog1 nil (format *error-output* "load-aut-spec parse error ~A: ~A~%" (token c) (message c))))
    (all-errors ()
      (prog1 nil (format *error-output* "error~%")))))

(defun build-spec (name l)
  (let* ((signature (nth 0 l))
	 (vars (nth 1 l))
	 (trss (mapcar #'make-seqsys-from-trs (nth 2 l)))
	 (automata (nth 3 l))
	 (tautomata (nth 4 l))
	 (termsets (nth 5 l))
	 (terms (nth 6 l))
	 (spec (make-spec name signature vars)))
    (dolist (trs trss)
      (add-trs trs spec))
    (when trss
      (setf (trs spec) (first trss)))
    (dolist (aut automata)
      (add-automaton aut spec))
    (when automata
      (setf (automaton spec) (first automata)))
    (dolist (taut tautomata)
      (add-tautomaton taut spec))
    (when tautomata
      (setf (tautomaton spec) (first tautomata)))
    (dolist (set termsets)
      (add-termset set spec))
    (dolist (term terms)
      (add-term term spec))
    (when termsets
      (setf (termset spec) (first termsets)))
    (when terms
      (setf (term spec) (first terms)))
    spec))

(defun write-automata (spec stream)
  (maphash (lambda (key value)
	     (declare (ignore key))
	     (show value stream)
	     (format stream "~%")
	     )
	   (table (automata spec))))

(defun write-term (term stream)
  (when term
    (format stream "Term ")
    (show term stream)
    (format stream "~%")))

(defun write-terms (spec stream)
  (maphash (lambda (key value)
	     (declare (ignore key))
	     (write-term value stream))
	   (table (terms spec))))

(defun write-termset (termset stream)
  (when termset
    (format stream "Termset ")
    (write-name (name termset) stream)
    (format stream " ")
    (display-sequence (get-terms termset) stream)
    (format stream "~%~%")))

(defun write-termsets (spec stream)
  (maphash (lambda (key value)
	     (declare (ignore key))
	     (write-termset value stream))
	   (table (termsets spec))))

(defun write-trs (trs stream)
    (format stream "TRS ")
    (write-name (name trs) stream)
    (format stream "~%")
    (format stream "~A~%" (get-rules trs))
    )

(defun write-trss (spec stream)
  (maphash (lambda (key value)
	     (declare (ignore key))
	     (write-trs value stream))
	   (table (trss spec))))

(defun write-spec (spec stream)
  (format stream "Ops ~A~%~%" (signature spec))
  (when (trs spec)
    (let ((vars (vars-of (get-rules (trs spec)))))
      (when vars
	(format stream "Vars ")
	(display-sequence vars stream)
	(format stream "~%~%"))))
  (write-trss spec stream)
  (write-automata spec stream)
  (write-termsets spec stream)
  (write-terms spec stream)
  (format stream "~%")
  )

(defun add-spec (spec specs)  
  (setf (gethash (name spec) specs) spec))

(defun write-current-spec (stream)
  (write-spec *spec* stream))

(defun add-current-spec ()
  (add-spec *spec* (table *specs*)))

(defun get-spec (name)
  (gethash name (table *specs*)))

(defun list-specs ()
  (list-keys (table *specs*)))

(defun set-current-spec (spec)
  (setf *spec* spec)
  (add-current-spec))

(defun back-up-file (file)
  (when (probe-file file)
      (let ((nfile (concatenate 'string file ".bak")))
	(back-up-file nfile)
	(rename-file file nfile))))

(defun save-spec (file)
  (back-up-file (absolute-filename file))
  (with-open-file (foo (absolute-filename file) :direction :output)
    (and foo (progn (write-current-spec foo) t))))
