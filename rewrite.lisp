

(in-package :om)

(defpackage "REWRITE"
    (:nicknames :rw)
    (:use "COMMON-LISP" "CL-USER" "OM-API"  "LISPWORKS" "HCL" "OM-LISP")
    (:import-from "CL-USER"))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc 
 #'(lambda (file) (om::compile&load (om-relative-path (cons "src" (car file)) (cadr file))))
 '((("rewrite") "subdivision") 
   (("rewrite") "transform") 
   (("rewrite") "general")
   (("rewrite") "interface") 
   (("rewrite") "polyphony")
   
   (("om-autowrite") "om-autowrite")
   
   ))

;--------------------------------------------------
; Setting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpackage-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library
 '(("Rewrite" 
    (("general" nil nil (rw::quantify rw::to-omtree rw::from-omtree rw::gcdize) nil)
     ("interface" nil (rw::rtree rw::transformation) (rw::display-tree rw::tree-transform) nil)
     ("polyphony" nil nil (rw::voices-separation rw::voices-remove-overlap) nil))
    nil nil nil)
   )
 (find-library "rewrite"))


(set-lib-release 0.1) 


(print "
;;;===================================================
;;;
;;; rhythm library for OM
;;;
;;;===================================================
")

