(in-package :common-lisp-user)

(defpackage :general
  (:use :common-lisp)
  (:export #:normalize-string
	   #:find-all
	   #:mappend
	   #:arrange-list
	   #:arrange
	   #:combinaison
	   #:permutations
	   #:distribute
	   #:distribute-set
	   #:cartesian-product
	   #:powerset
	   #:not-null-powerset
	   #:flatten
	   #:nflatten
	   #:nmapcar
	   #:adjoin-sort
	   #:member-sort
	   #:union-sort
	   #:intersection-sort
	   #:difference-sort
	   #:ordered-equal
	   #:suite
	   #:list-keys
	   #:list-values
	   #:avec-temps
	   #:evaluate-time
	   #:iota
	   #:integer-partition
	   #:remove-duplicates-except-pairs
	   #:lexically-ordered-p
	   #:lexically-strictly-ordered-p
	   #:lvbits
	   #:onep
	   #:annotation
	   #:annotation-mixin
	   #:erase-annotation
	   #:hash-reduce
	   ))
