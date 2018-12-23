(in-package :nautowrite)
(defvar *det-aut-c* t)

(defun cstates (aut-c)
  (contents (compute-target (bullet-term) aut-c)))

(defun make-d-transitions-for-constants (aut-c csignature aut-b )
  (mapc
   (lambda (sym)
       (let*
	   ((bc (contents (compute-target (build-term sym) aut-b)))
	    (sc (apply-transition-function sym '() (transitions-of aut-c)))
	    (pc (if (bc-redex-p bc)
		    *cstates* ; redex
                    nil)))
	 (funcall (if *det-aut-c*
		      #'add-dtransition-to-transitions
		      #'add-transition-to-transitions)
		  (list sym) (make-dstate sc bc pc :deterministic *det-aut-c*) *global-transitions*)))
   (signature-symbols csignature)))

(defun create-prh (root arg bc ctransitions)
  (multiple-value-bind (res p1)
      (p1rh root arg ctransitions)
    (if (null res)
	(values nil nil)
	(values t
		(if (bc-redex-p bc)
		    (adapt-p-when-redex p1)
		    p1)))))

(defun adapt-p-when-redex (p1)
  (if *det-aut-c*
      (union-sort-casted-states *cstates* p1)
      (g-adapt-p-when-redex p1)))
;; NEW
;; ??

(defun p1rh (root arg ctransitions)
  (let* ((lds (mapcar #'s-comp arg))
	 (ldp (mapcar #'p-comp arg)))
;;     (format *error-output*
;; 	    "p1rh root ~A  lds ~A ldp ~A~%" root lds ldp)
    (multiple-value-bind (res rhap)
	(rhs-avec-un-p root lds ldp ctransitions)
;;      (format *error-output* "res rhs-avec-un-p ~A  rhap ~A ~%" res rhap)
      (if res
	  (values t (transform-rhap rhap))
	  (values nil nil)))))

(defun transform-rhap (rhap)
  (if *det-aut-c*
      (sort-casted-states rhap)
      (g-transform-rhap rhap)))

;; pour f (S1,S2,S3) (P1,P2,P3) 
;; donne (f(p11,S2,S3)^ f(p12,S2,S3)^ 
;;        f(S1,p21,S3)^ f(S1,p22,S3)^
;;        f(S1,S2,p31)^ f(S1,S2,p32)^) pour
;; tout Pi non nul 
(defun rhs-avec-un-p (symbol lds ldp ctransitions)
;;   (format *error-output*
;;	    "rhs-avec-un-p symbol ~A  lds ~A ldp ~A~%" symbol lds ldp)
  (do
   ((lds1 nil (append lds1 (list (car lds2))))
    (lds2 lds (cdr lds2))
    (ldp ldp (cdr ldp))
    (aux nil))
   ((endp lds2) (values t (state-remove-duplicates aux)))
    (unless (null (car ldp))
      (let ((ll (new-p-rh symbol lds1 lds2 (car ldp) ctransitions)))
	(if (member nil ll)
	    (return (values nil nil))
	    (setf aux (nconc aux ll)))))))

(defun expand (gs)
;;  (format *error-output* "type-of ~A~%"(type-of gs))
  (if *det-aut-c*
      gs
      (mapcar
       (lambda (casted-state)
	 (make-container-from-state casted-state))
       gs)))

;; lds1 S-1 ...S-i-1
;; lds2 S-i+1 ... S-n
;; p P-i
;; retourne la liste des  gstates obtenus 
;; par application des regles 
;; sur (symbol S-1 ...S-i-1 {q} S-i+1 ... S-n)
;; pour chaque element q de P-i

(defun new-p-rh (symbol lds1 lds2 p ctransitions)
;;   (format *error-output*
;;	    "new-p-rh symbol ~A  lds1 ~A lds2 ~A p ~A~%" symbol lds1 lds2 p)
   (let* ((args
	   (mapcar
	    (lambda (q) (append lds1 (list q) (cdr lds2)))
	    (expand p))))
;;     (format *error-output* "terms ~A" args)
     (let ((targets
	    (mapcar
	     (lambda (arg)
	       (apply-transition-function-gft symbol arg ctransitions))
	     args)))
;;       (format *error-output* "targets ~A~%" targets)
;; si *det-aut-c* liste de gstates sinon liste de containers
       (if *det-aut-c*
	   (casted-state-remove-duplicates targets)
	   (my-remove-duplicates targets)))))

(defun create-transitions (key bc srh prh)
  (if *det-aut-c*
      (det-create-transitions key bc srh prh)
      (g-create-transitions key bc srh prh)))

(defun gen-d-transitions (root arg ctransitions aut-b)
  (let* ((srh (apply-transition-function-gft root (mapcar #'s-comp arg) ctransitions))
	 (bp (b-projection arg))
	 (bc (contents (apply-transition-function-gft
			root bp
			(transitions-of aut-b)))))
    (unless (null srh)
      (multiple-value-bind (res prh)
	  (create-prh root arg bc ctransitions)
	(unless (null res)
	  (create-transitions (cons root arg) bc srh prh))))))

(defun adapt-d-transitions-for-one-symbol (symbol afai ctransitions aut-b)
;;  (format t "adapt-d-transitions-for-one-symbol ~A ~A~%" symbol (length afai))
  (mapc
   (lambda (arg)
     (gen-d-transitions symbol arg ctransitions aut-b))
   afai))

(defun adapt-d-transitions (ncsignature states newstates ctransitions aut-b)
  (let* ((max-arity (max-arity ncsignature))
	 (afa (arrange-for-arities-and-filter
	       (append states newstates) max-arity states ncsignature)))
    (incf *d-iterations*)
    (mapc
     (lambda (symbol)
       (let* ((arity (arity symbol))
	      (afai (aref afa arity)))
	 (adapt-d-transitions-for-one-symbol
	  symbol
	  afai
	  ctransitions
	  aut-b)))
     (signature-symbols ncsignature))))

(defun make-d-transitions (signature aut-c aut-b partial)
  (let ((ctransitions (transitions-of aut-c)))
    (make-d-transitions-for-constants aut-c (constant-signature signature) aut-b)
    (do* ((states (make-empty-ordered-container))
	  (newstates (cright-handsides *global-transitions*)
	    (container-difference (cright-handsides *global-transitions*) states)))
	((or
	  (empty-container-p newstates)
	  (and partial
	       (find-if (lambda (dstate) (is-final-state-d dstate (get-finalstates aut-c)))
			(contents newstates)))))
      (adapt-d-transitions
       (non-constant-signature signature) (contents states) (contents newstates) ctransitions aut-b)
      (container-nunion states newstates))))

(defun make-d-automaton (aut-c aut-b &key deterministic (partial nil))
  (when deterministic 
    (assert (deterministic-p aut-c)))
  (let ((*det-aut-c* deterministic))
    (with-new-transitions
      (setf *d-iterations* 0)
      (init-dstates)
      (let*
	  ((*st-redex*
	    (find-casted-state-from-state
	     (make-term-state (build-term *l-symbol*)) (get-states aut-b))); 0
	   (*st-reducible*
	    (find-casted-state-from-state
	     (make-term-state (build-term *r-symbol*)) (get-states aut-b))) ; 1
	   (*cstates* (cstates aut-c))
	   (signature (remove-bullet-symbol (signature aut-c))))
	(make-d-transitions
	 signature
	 aut-c
	 aut-b
	 partial)
	(let* ((states (states-from-states-table (states-table *global-transitions*)))
	       (fs (container-remove-if-not
		    (lambda (dstate)
		      (is-final-state-d dstate (get-finalstates aut-c)))
		    states)))
	  (nreduce-automaton
	   (make-automaton
	    *global-transitions*
	    :name (format nil "aut-d-~A-~A-~A"
			  (name (trs *spec*))
			  (get-approx-name (seqsys-approx (trs *spec*)))
			  (if deterministic "det" "ndet"))
	    :signature signature
	    :finalstates fs)))))))
