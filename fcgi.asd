; Copyright (C) 2006 by Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

; Special Thanks to Edward Marco Baringer for his help on #lisp on FreeNode
; about setting up a testing environment and his unit test framework FiveAM

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fcgi-system)
    (defpackage :fcgi-system
      (:documentation "ASDF System package for FCGI")
      (:use :common-lisp :asdf))))

(in-package :fcgi-system)

(defsystem fcgi
  :name "fcgi"
  :version "0.01"
  :maintainer "Rüdiger Sonderfeld <ruediger@c-plusplus.de>"
  :author "Rüdiger Sonderfeld <ruediger@c-plusplus.de>"
  :licence ""
  :description "FastCGI (FCGI)"
  :long-description "A Common Lisp library for the FastCGI 1.0 specification"
  :serial t
  :components ((:file "package")
	       (:file "misc")
	       (:file "binary-mapper")
	       (:file "fcgi-data")
	       (:file "fcgi")))

(defsystem fcgi-test
  :components ((:module :unit-tests
		:components ((:file "unit-test-suite")
			     (:file "misc-test")
			     (:file "binary-mapper-test")
			     (:file "fcgi-data-test")
			     (:file "fcgi-test"))))
  :depends-on (:fcgi :FiveAM))

(defmethod asdf:perform :after ((op asdf:test-op)
			 (system (eql (asdf:find-system :fcgi))))
  (asdf:oos 'asdf:load-op :fcgi-test)
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :fcgi))

(defmethod asdf:operation-done-p ((op asdf:test-op)
				  (c asdf:component))
  (declare (ignore op c))
  nil)
