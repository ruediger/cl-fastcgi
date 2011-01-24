(in-package :fcgi)

(defpackage :fcgi-test
  (:use :common-lisp :fcgi :it.bese.FiveAM))

(5am:def-suite :fcgi :description "fcgi test suite")
