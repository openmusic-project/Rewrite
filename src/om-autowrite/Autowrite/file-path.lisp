(in-package :nautowrite)
(defvar *data-relative-directory* "Data/")

(defun default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
  #+cmu (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #-(or allegro clisp cmu cormanlisp lispworks lucid) (truename "."))

(defun initial-data-directory ()
  (concatenate 'string
	       (namestring 
;;		(default-directory)
		nautowrite-system::*autowrite-directory*
		)
	       *data-relative-directory*))
  
(defvar *data-directory* (initial-data-directory))

