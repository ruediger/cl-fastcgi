(in-package :fcgi)

(defconstant +fcgi-max-connections+ 16)

(binary-mapper fcgi-header
	       version
	       conn-type
	       (request-id compose-2)
	       (content-length compose-2)
	       padding-length
	       reserved)

(defconstant +fcgi-begin-request+ 1)
(defconstant +fcgi-abort-request+ 2)
(defconstant +fcgi-end-request+ 3) 
(defconstant +fcgi-params+ 4)
(defconstant +fcgi-stdin+ 5)
(defconstant +fcgi-stdout+ 6)
(defconstant +fcgi-stderr+ 7)
(defconstant +fcgi-data+ 8)
(defconstant +fcgi-get-values+ 9)
(defconstant +fcgi-get-values-result+ 10)
(defconstant +fcgi-unknown-type+ 11)
(defconstant +fcgi-maxtype+ 11)
(defconstant +fcgi-null-request-id+ 0)
(defconstant +fcgi-version+ 1)

(binary-mapper fcgi-begin-request-body
	       (role compose-2)
	       flags
	       reserved0 reserved1 reserved2 reserved3 reserved4)

(defconstant +fcgi-keep-conn+ 1)
(defconstant +fcgi-responder+ 1)
(defconstant +fcgi-authorizer+ 2)
(defconstant +fcgi-filter+ 3)

(binary-mapper fcgi-end-request-body
	       (app-status compose-4)
	       protocol-status
	       reserved0 reserved1 reserved2)

(defconstant +fcgi-request-complete+ 0)
(defconstant +fcgi-cant-mpx-conn+ 1)
(defconstant +fcgi-overloaded+ 2)
(defconstant +fcgi-unknown-role+ 3)

(binary-mapper fcgi-unknown-type-body
	       conn-type
	       reserved0 reserved1 reserved2 reserved3 reserved4 reserved5
	       reserved6)

(defun read-name-value-pairs (buffer &optional (handle-cons #'(lambda (c) c)))
  "Converts Name-Value-Pairs in buffer into a cons and calls handle-cons for
   each (returning a list of the return values of handle-cons) or returns
   a list of cons."
  (loop
     with pos = 0
     with name-length
     with value-length
     until (>= pos (length buffer))
     do (progn
	  (setf name-length (aref buffer pos))
	  (when (= (ash name-length -7) 1) ; 31 bit name-length?
	    (setf name-length (to-31bit buffer pos))
	    (incf pos 3))
	  (setf value-length (aref buffer (incf pos)))
	  (when (= (ash value-length -7) 1) ; 31 bit value-length?
	    (setf value-length (to-31bit buffer pos))
	    (incf pos 3)))
     collect (funcall handle-cons
		      (cons (map 'string #'code-char
				 (subseq buffer (incf pos)
					 (incf pos name-length)))
			    (map 'string #'code-char
				 (subseq buffer pos
					 (incf pos value-length)))))))

(defun write-name-value-pair (pair)
  "Converts a cons into a Name-Value-Pair."
  (let ((name (car pair)) (value (cdr pair)))
    (when (stringp name)
      (setf name (str-to-vector-ub8 name)))
    (when (stringp value)
      (setf value (str-to-vector-ub8 value)))
    (concatenate '(vector (unsigned-byte 8))
		 (length-to-binary (length name))
		 (length-to-binary (length value))
		 name
		 value)))
