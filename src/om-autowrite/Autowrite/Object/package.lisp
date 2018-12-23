(in-package :common-lisp-user)

(defpackage :object
  (:use :common-lisp :general :names)
  (:export
   #:show
   #:compare-object
   #:display-sequence
   #:find-object
   #:size
   #:my-assoc
   #:my-member
   #:my-remove
   #:my-remove-duplicates
   #:my-delete
   #:my-delete-duplicates
   #:my-adjoin
   #:my-subsetp
   #:compare-set
   #:my-union
   #:my-nunion
   #:my-intersection
   #:my-setdifference
   #:my-nsetdifference
   #:mapunion
   #:no-duplicates
   #:named-object
   #:name
   #:rename-object
   ))
