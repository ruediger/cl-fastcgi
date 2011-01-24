(in-package :fcgi-test)

(def-suite :misc-test :in :fcgi)

(in-suite :misc-test)

(test to-string
  (is (string= (fcgi::to-string 'hello) "HELLO")))

(test str-to-vector-ub8
  (let ((eq-to (make-array 5 :element-type '(unsigned-byte 8)
			   :initial-contents (list (char-code #\h)
						   (char-code #\e)
						   (char-code #\l)
						   (char-code #\l)
						   (char-code #\o)))))
    (is (equalp (fcgi::str-to-vector-ub8 "hello") eq-to))))

(test to-31bit
  (is (= (fcgi::to-31bit #(0 0 0 #x7f) 0) #x7f))
  (is (= (fcgi::to-31bit #(#x80 0 0 #xff) 0) #xff)))

(test length-to-binary
  (let ((test-value (make-array 4 :element-type '(unsigned-byte 8)
				:initial-contents '(#x80 0 0 #xff))))
    (is (equalp (fcgi::length-to-binary #xff) test-value)))
  (let ((test-value (make-array 1 :element-type '(unsigned-byte 8)
				:initial-element #x7f)))
    (is (equalp (fcgi::length-to-binary #x7f) test-value))))

(test string-remove
  (is (string= (fcgi::string-remove "haello" 1 1) "hello"))
  (is (string= (fcgi::string-remove "helfooobaarlo" 3 8) "hello")))
