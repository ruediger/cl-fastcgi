(in-package :fcgi-test)

(def-suite :fcgi-data-test :in :fcgi)

(in-suite :fcgi-data-test)

(test read-name-value-pairs
  ; TODO: test with bigger input-data
  (let ((input-data (make-array 12 :element-type '(unsigned-byte 8)
				:initial-contents
				(concatenate 'list
					     '(#x05 #x05)
				    (loop for i across "hello"
				       collect (char-code i))
				    (loop for i across "hallo"
				       collect (char-code i))))))
    (is (equal (fcgi::read-name-value-pairs input-data)
	       '(("hello" . "hallo"))))
    (is (equal (fcgi::read-name-value-pairs input-data
			      #'(lambda (c)
				  (is (equal c '("hello" . "hallo")))
				  t))
	       '(t)))))


(test write-name-value-pair
  (let ((test-data (make-array 12 :element-type '(unsigned-byte 8)
			       :initial-contents
			       (concatenate 'list
					    '(#x05 #x05)
					    (loop for i across "hello"
					       collect (char-code i))
					    (loop for i across "hallo"
					       collect (char-code i))))))
    (is (equalp (fcgi::write-name-value-pair '("hello" . "hallo")) test-data))))
