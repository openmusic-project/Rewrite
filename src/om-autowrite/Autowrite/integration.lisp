(in-package :nautowrite)
(defparameter *approximation* 'growing-approximation)

(defun u-automaton (signature lhs aut-a)
  (let ((aut-b (make-matching-automaton lhs signature :strict t)))
    (disjoint-union-automaton aut-b aut-a)))

(defun make-u-automaton (signature lhs aut-a &key (bullet nil) (extra nil))
  (when bullet
    (setf signature (add-bullet-symbol signature)))
  (when extra
    (setf signature (add-extra-symbol signature)))
   (let ((usignature (merge-signature signature (signature aut-a))))
     (u-automaton usignature lhs aut-a)))

(defmethod nfold-constant-states ((automaton table-automaton))
  (let ((states (get-states automaton))
	(constant-symbols
	 (signature-symbols
	  (constant-signature (signature automaton)))))
    (flet ((aux (i)
	     (mapcan
	      (lambda (csym)
		(let ((s (find-casted-state-from-state
			  (make-indexed-state (make-term-state (build-term csym)) i)
			  states)))
		  (and s (list s))))
	      constant-symbols)))
      (let* ((zeros  (aux 0))
	     (ones (aux 1))
	     (rest (set-difference (contents states) (append zeros ones))))
	(let ((*states-mapping*  (pairlis (append zeros rest) (append ones rest))))
	  (print *states-mapping*)
	  (napply-states-mapping automaton))))))

(defun ndet-saturation-c-automaton (sys aut-u)
  (let ((aut-c (nreduce-automaton (nfold-constant-states (saturation sys aut-u)))))
;;  (let ((aut-c (saturation sys aut-u)))
    (rename-object
     aut-c
     (format nil "aut-c-jacquemard-~A~A~A~A"
	     (name (trs *spec*))
	     (if (in-signature-extra
		  (signature aut-c)) "@-" "")
	     (get-approx-name (seqsys-approx (trs *spec*)))
	     (if (in-signature-bullet (signature aut-u)) "-o" "")))))

(defun make-c-automaton (sys aut-u deterministic &key (simplify nil))
  (if deterministic
      (det-saturation-automaton sys aut-u :simplify simplify)
      (ndet-saturation-c-automaton sys aut-u)))
