(in-package :names)

(defvar *names* nil "list of already defined names")
(defvar *special-chars* (list #\- #\( #\) #\> #\)))

(defun init-names ()
  (setf *names* nil))

(defun make-name (name)
  (or
    (car (member name *names* :test #'equal))
    (progn
      (push name *names*)
      name)))

(defun strong-name (name)
   (if (intersection (coerce name 'list) *special-chars*)
       (format nil "\"~A\"" name)
       (format nil "~A" name)))

(defun write-name (name stream)
  (format stream (strong-name name)))

(defun compose-name (name1 name2)
   (concatenate 'string name1 (add-parentheses-name name2)))

(defun prefix-length (prefix name)
 (string<= prefix name))

(defun trim-name (n name &key (from-end nil))
  (if from-end
      (subseq name 0 (- (length name) n))
      (subseq name n)))

(defun decompose-name (prefix name &key (from-right nil))
  (if from-right
      (reverse (decompose-name (reverse prefix) (reverse name)))
      (let ((n (prefix-length prefix name)))
	(and n (= n (length prefix)) (remove-parentheses-name (trim-name n name))))))

(defun add-parentheses-name (name)
  (if (member #\- (coerce name 'list))
      (concatenate 'string "(" name ")")
      name
      ))

(defun remove-parentheses-name (name)
  (string-right-trim
   ")"
   (string-left-trim "(" name)))
