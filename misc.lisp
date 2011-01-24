(in-package :fcgi)

(defun to-string (n)
  (format nil "~A" n))

(defun str-to-vector-ub8 (n)
  (declare (string n))
  "WARNING: This function can't handle strings with character size bigger than
   (unsigned-byte 8)!"
  (map '(vector (unsigned-byte 8)) #'(lambda (c) (char-code c)) n))

(defun to-31bit (buffer pos)
  (declare (type (vector (unsigned-byte 8)) buffer) (number pos))
  "Converts 4 bytes from buffer beginning at pos into a 31 Bit Fixnum"
  (+ (ash (logand (aref buffer pos) #x7f) 24)
     (ash (aref buffer (1+ pos)) 16)
     (ash (aref buffer (+ 2 pos)) 8)
     (aref buffer (+ 3 pos))))

(defun length-to-binary (value)
  (declare (number value))
  "Converts value into binary data.

   It either returns an array of 4 byte with 31 bit for value and the first bit
   as a flag set to 1 or returns an array of 1 byte with 7 bit for value and
   the first bit set to 0."
  (if (> value #x7f) ; does length fit into 7 Bit or 31 Bit required?
      (let ((buffer (make-array 4 :element-type '(unsigned-byte 8))))
	(setf (aref buffer 0) (logior (ash value -24) #x80)) ; activate bit 32
	(setf (aref buffer 1) (ash value -16))
	(setf (aref buffer 2) (ash value -8))
	(setf (aref buffer 3) value)
	buffer)
      (make-array 1 :element-type '(unsigned-byte 8)
		  :initial-element value)))

(defun string-remove (string pos n)
  (declare (string string) (number pos n))
  "removes `n' characters from the sequence `string' at position `pos'"
  (concatenate 'string (subseq string 0 pos) (subseq string (+ pos n))))

; copied from trivial-sockets library (see http://verisons.telent.net/cgi-bin/darcs.cgi/trivial-sockets/sbcl.lisp?c=annotate)
(defun resolve-hostname (name)
  (cond
    ((eql name :any)  #(0 0 0 0))
    ((typep name '(vector * 4)) name)
    (t (car (sb-bsd-sockets:host-ent-addresses
	     (sb-bsd-sockets:get-host-by-name name))))))