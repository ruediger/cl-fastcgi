; Copyright (C) 2006 by Rüdiger Sonderfeld <ruediger@c-plusplus.de>
; see the file COPYING for license information

(defpackage :fcgi
  (:use :common-lisp :sb-bsd-sockets)
  (:export "LS-READER"
	   "ACCEPT"
	   "SETUP-ENVIRONMENT"
	   "WRITE-STDOUT"
	   "WRITE-STDERR"
	   "END-REQUEST"
	   "EXPAND-ESCAPE-SEQUENCES"))
