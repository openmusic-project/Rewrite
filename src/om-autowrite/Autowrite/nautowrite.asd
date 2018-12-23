;;; -*- Mode: Lisp -*-
;;;  (c) copyright 2006 by
;;;           Irène Durand (idurand@labri.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; ASDF system definition for Autowrite.

(defpackage :nautowrite-system
  (:use :asdf :common-lisp))

(in-package :nautowrite-system)

(defparameter *autowrite-directory* (directory-namestring *load-truename*))

;; (format t "load-truename: ~A ~%" *load-truename*)
(format t "autowrite-directory is ~A ~%" *autowrite-directory*)

(defsystem :nautowrite
  :description "Autowrite: Term rewriting and Automata"
  :name "autowrite"
  :version "4.0"
  :author "Irene Durand <idurand@labri.fr>"
  :licence "General Public Licence"
  :serial t
  :components (
	    (:module "General"
		     :components ((:file "package")
				  (:file "general")
				  (:file "hashtable")
				  (:file "time")
				  (:file "annotation")
				  )
		     :serial t)
	    (:module "Names"
		     :components ((:file "package")
				  (:file "names"))
		     :serial t)
	    (:module "Object"
		     :components ((:file "package")
				  (:file "object")
				  (:file "named-object"))
		     :serial t)
	    (:module "Symbols"
		     :components ((:file "package")
				  (:file "arity")
				  (:file "symbols")
				  (:file "aux-symbol")
				  (:file "signature")
				  (:file "signed-object")
				  (:file "annotated-symbols")
				  )
		     :serial t)
	    (:module "Term"
		     :components ((:file "package")
				  (:file "abstract-term")
				  (:file "variables")
				  (:file "aux-var")
				  (:file "vars-subst")
				  (:file "path")
				  (:file "position")
				  (:file "term")
				  (:file "term-variable")
				  (:file "term-pos")
				  (:file "omega-terms")
				  (:file "aterm")
				  )
		     :serial t)
	    (:file "package")
	    (:file "globals")
	    (:file "arities")
	    (:file "unify")
	    (:file "terms")
	    (:file "state-contents")
	    (:file "abstract-state")
	    (:file "state")
	    (:file "symbol-state")
	    (:file "rule")
	    (:file "rules")
	    (:file "flat-term-state")
	    (:file "term-state")
	    (:file "container")
	    (:file "gstate")
	    (:file "target")
	    (:file "transition")
	    (:file "simple-table")
	    (:file "dag-table")
	    (:file "states-table")
	    (:file "abstract-transitions")
	    (:file "transitions")
	    (:file "sym-transitions")
	    (:file "simple-transitions")
	    (:file "fly-transitions")
	    (:file "term-rules")
	    (:file "trs")
	    (:file "termset")
	    (:file "generation")
	    (:file "index-point")
	    (:file "abstract-automaton")
	    (:file "table-automaton")
	    (:file "bvstate")
	    (:file "determinize")
	    (:file "states-mapping")
	    (:file "duplicate-automaton")
	    (:file "automata")
	    (:file "tuple-state")
	    (:file "intersection-emptyness")
	    (:file "term-to-state")
	    (:file "accessibilite")
	    (:file "boolean-op-automaton")
	    (:file "product")
	    (:file "union")
	    (:file "sstates")
	    (:file "intersection")
	    (:file "equivalence-classes")
	    (:file "quotient")
	    (:file "minimization")
	    (:file "simplification")
	    (:file "epsilon")
	    (:file "tautomate")
	    (:file "forward-branching")
	    (:file "index-tree")
	    (:file "salinier")
	    (:file "lexer")
	    (:file "parser")
	    (:file "nfautomate")
	    (:file "bprime-automate")
	    (:file "dstate")
	    (:file "specification")
	    (:file "gsaturation")
	    (:file "nlsaturation")
	    (:file "gd-automate")
	    (:file "det-dautomate")
	    (:file "dautomate")
	    (:file "approximation")
	    (:file "integration")
	    (:file "file-path")
	    (:file "input")
	    (:file "output")
	    (:file "init-autowrite")
	    (:file "context")
	    (:file "lang-context")
	    (:file "needed-redex")
	    (:file "termination")
	    (:file "com-term")
	    (:file "com-termset")
	    (:file "com-trs")
	    (:file "com-automaton")
	    (:file "com-reduction")
	    (:file "com-approximation")
	    (:file "signature-mapping")
	    (:file "vmodification")
	    (:file "hvmodification")
	    (:file "vprojection")
	    (:file "vcylindrification")
	    (:file "vhomomorphisme")
	    (:file "cprojection")
	    (:file "fly-automaton")
	    (:file "fly-to-table")
	    (:file "vbits-homomorphisms")
	    (:file "jeu-de-tests"))
  :serial t)

(pushnew :nautowrite *features*)
