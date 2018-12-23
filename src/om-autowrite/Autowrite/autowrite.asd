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

(require :nautowrite)

(defpackage :autowrite-system
  (:use :asdf :common-lisp))

(in-package :autowrite-system)

(defsystem :autowrite
  :description "Graphical Inferface for Autowrite"
  :version "3.0"
  :author "Irene Durand <idurand@labri.fr>"
  :licence "Public Domain"
  :depends-on (:mcclim :nautowrite)
  :serial t
  :components ((:file "gautowrite")
	       (:file "interface-variables")
	       (:file "process")
	       (:file "utils-interface")
	       (:file "toy")
	       (:file "presentations")
	       (:file "completion")
	       (:file "processes")
	       (:file "interface")
))

(pushnew :autowrite *features*)
