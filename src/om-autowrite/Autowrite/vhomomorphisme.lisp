(in-package :nautowrite)

(defgeneric vhomomorphism (o h)
  (:documentation "transforme le symbole c de l' objet o en
    (c1 ... ck) avec h(c) -> {c1 ... ck}"))

(defmethod vhomomorphism ((signed-object signed-object) h)
  (let ((*modification-function* h))
    (vmodification signed-object)))
