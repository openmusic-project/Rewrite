(in-package :nautowrite)

(defun gen-terms-depth (d signature)
  (if (= d 0) 
      (build-zeroary-terms-from-symbols (constant-signature signature))
      (let ((sub (gen-terms-depth (- d 1) signature)))
	(my-union (gen-terms sub (non-constant-signature signature)) sub))))

(defun gen-terms-with-symbol (l sym)
  (mapcar
   #'(lambda (x) (build-term sym x))
   (arrange l (arity sym))))

(defun gen-terms (sub ncsignature)
  (mappend #'(lambda (x) (gen-terms-with-symbol sub x)) (signature-symbols ncsignature)))

(defun gen-normal-forms-depth (d prog signature)
  (if (= d 0) 
      (filter-non-redex
       (build-zeroary-terms-from-symbols
			 (constant-signature signature)) prog)
      (let ((sub (gen-normal-forms-depth (- d 1)  prog signature)))
	(my-union (filter-non-redex (gen-terms sub (non-constant-signature signature)) prog) sub))))
