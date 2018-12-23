; Author : Pierre Donat-Bouillud

(in-package :rw)


;TODO : améliorer la gestion des tests ?

(print "Conversion tests")

(tree-to-om '(:n :r (:n :r) :r))

(tree-to-om '(:n := (:n :n :n) :n)) ; Tricky example of quarter note triplet

(tree-to-om '(:n := ( :s :n) :r)) ; Example with a slur

(to-omtree '( (:n :r (:n :r) :r) (:n := (:= :n :n) :r))) ; Deux mesures

;Quelques cas pathologiques :

(to-omtree '( (:n :r (:s :n :s) :s)) )

(to-omtree '((:r :r ((:r :r :r :n) :s :s :s) :s)  (:s) (:s (:s :s :s (:s :r :r :r)) :r :r) (:r :r (:r :r :r (:s :r :r :n)) :s)))

(to-omtree '( (:n := := :s)))

;Should find something like ( (n r (n r) r) (n = (= n n) r))
(from-omtree '(? (((4 4) (1 -1 (1 (1 -1)) -1)) ((4 4) (1 (2 (2 1)) -1)))))

(expand-symb 3); (3 1) -> (= = n n)

(expand-symb 3 '(1 2)); ((3 (1 2))  1) -> ( = = (n = n) n)

(pulse-from-om '(1 -1 (1 (1 -1)) -1))

(from-omtree '(? (((4 4) (2 -2 (2 (1 -1)) -2))))) ; Vérifier si canonize est bien placée

;Identité ? Non, car renvoie une représentation canonique
(to-omtree (from-omtree '(? (((4 4) (2 -2 (2 (1 -1)) -2)) ((4 4) (1 (2 (2 1)) -1))))))

(print "Transform tests")

;;merge-rs and merge-ns
(apply-transform '((:n :n) (:s :s :s :s)) 'merge-rs )

(apply-transform '( (:n :n (:s :s :s :s) :n) (:s :s :s :s ) (:n (:s :s) :r (:r :r))) 'merge-rs :measure t)

(apply-transform '(:n (:n :s) :r) 'merge-ns)

(apply-transforms '( (:n :n (:n :s :s :s) :n) (:s :s :s :s ) (:n (:s :s) :r (:r :r))) '(merge-ns merge-rs) :measure t)


;;gen-=
(gen-= '( :n :n :s :s :n) nil)
(apply-transform '( :n :n :s :s :n) 'gen-=  )

(apply-transform '(:r (:r :r :r :n) :s :s) 'gen-=)

(apply-transform '( (:n :s :s :s ) (:n :n )) 'gen-=  :measure t)

(apply-transform '((:s :s :s (:s :s (:s :s :r :r) :r)) :r :r :r) 'gen-=)

;;move-up

(apply-transform '((:r (:r :r :r :n) :s :s) (:s ((:s :s :s :r) :r :r :r) :r :r) (:n) :s) 'move-up)

(apply-transform '(((:r (:r :r :r :n) :s :s) (:s ((:s :s :s :r) :r :r :r) :r :r) (:n) :s) ((:s (:s (:s :s :r :r) :r :r) :r :r) (:r (:r (:r :r := :n) :s :s) :s :s) :s :s) ((:s :s :s (:s :s (:s :s :r :r) :r)) :r :r :r) (:r :r :r :r)) 'move-up)

;;resubdivize

(apply-transform '((:n :s :n :s))  'resubdivize )

(apply-transform '((:n :s :n :s))  'resubdivize ) ;-> ((:n))  puis move-up <=> move-ns !

;dilation

(apply-transform '((:r :r :n :n :r)) 'dilate)


;cut sides

(apply-transform  '(((:n :r :n :r :n))) 'cut-sides)

;correct-slurs

(apply-transform '((:n :n :n :r) (:s :s :r :n)) 'correct-slurs )

(apply-transform '(((:n :n :n (:n :n (:n :r))) (:s :s :r :n) (:n :n :n :n))) 'correct-slurs)


(apply-transform '(((:s :r :r :r ) ( :r (:r (:r :r :r :r) :r :r) :s :s ) :s :s )) 'correct-slurs)


(print "Subdivision tests")

(print "  1) Grid tests")
(equalp (build-grid '(300 2000 5345) '( 1000 2345 3567.89)) (build-grid '(300 2000 5345) '( 1000 2345 3567.89) :strategy (make-instance 'amount-strategy :amount 0.3)))

(multiple-value-bind (grid subdi-duration)  (build-grid '(300 2000 5345) '( 1000 2345 3567.89)   :strategy (make-instance 'silence-strategy)) 
  (print(float  subdi-duration))
  (time-grid grid (float subdi-duration)))


(print "  2) Subdivision tests")

(let ((tree  (basic-subdivision '(300 2000 5345) '( 1000 2345 3567.89):subdivision 2 :depth 6 )))
;(explore-tree #'(lambda(element) (equalp element :r) ) tree )
;(print "AVANT")
;(print tree)
;(print "APRES")
(let ((merged-tree (apply-transforms tree '(merge-ns merge-rs) :measure t)))
  ;(print merged-tree)
  ;(to-omtree merged-tree))))
  (to-omtree (mapcar #'(lambda (mes) (typecase mes (list mes) (t (list mes))))  merged-tree))))

(print "  3) Separation test")

(add-note '((6000)) '((0)) '((0)) 6000 10 1000)

(add-note '((6000)) '((0)) '((200)) 5000 100 1000)

(add-note () () () 900 300 1000)

(add-note '((600)) '((10)) '((500)) 5000 1000 200)


(voices-separation  '(6000 5000 7000) '(10 1000 2010) '(1000 2000 100))

(voices-separation '(6000 5000 7000) '(10 1000 2010) '(500 200 300))

(voices-remove-overlap '(6000 5000 7000) '(10 500 1000) '(500 600 500))
