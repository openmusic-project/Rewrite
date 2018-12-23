(in-package :nautowrite)

(defgeneric needed-redex-position-p (p term aut-c)
  (:documentation "true if position P in TERM is needed with help of AUT-C"))

(defgeneric needed-redex-positions (term sys &key extra)
  (:documentation "list of the positions of the needed redexes of TERM according to the system SYS possibly with signature extended with the extra-symbol @"))

(defgeneric redex-positions (term sys)
  (:documentation "list of the positions of the redexes of TERM according to the system SYS"))

(defgeneric outermost-redex-positions (term sys)
  (:documentation "list of the positions of the outermost redexes of TERM according to the system SYS"))

(defmethod needed-redex-position-p ((p my-position) (term term) (aut-c abstract-automaton))
  (not (recognized-p (replace-term-at-position term p (bullet-term)) aut-c)))
 
(defmethod needed-redex-positions ((term term) (sys seqsys) &key (extra nil))
  (let ((aut-c (seqsys-aut-c sys :extra extra :bullet t))
	(rp (redex-positions term sys)))
    (remove-if-not
     (lambda (p) (needed-redex-position-p p term aut-c))
     rp)))
 
(defmethod redex-positions ((term term) (sys seqsys))
    (redex-positions-lhs term (get-lhs sys)))

(defmethod outermost-redex-positions ((term term) (sys seqsys))
  (outermost-redex-positions-lhs term (get-lhs sys)))

