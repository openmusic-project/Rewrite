(in-package :common-lisp-user)

(defpackage :names
  (:use :common-lisp)
  (:export #:init-names
	   #:make-name
	   #:strong-name
	   #:write-name
	   #:compose-name
	   #:prefix-length
	   #:trim-name
	   #:decompose-name
))
