(in-package :fcgi-test)

(def-suite :fcgi-binary-mapper-test :in :fcgi)

(in-suite :fcgi-binary-mapper-test)

(test binary-mapper
  "test binary-mapper"
  (fcgi::binary-mapper test-map
		       one-byte
		       (two-byte fcgi::compose-2)
		       (four-byte fcgi::compose-4)
		       one-byte-again)
  (let* ((byte-data #(1 #xaa #xcc #xff #xee #xdd #xcc #xff))
	 (map-obj (map-to-test-map byte-data)))
    (is (= (one-byte map-obj) 1))
    (is (= (two-byte map-obj) #xaacc))
    (is (= (four-byte map-obj) #xffeeddcc))
    (is (= (one-byte-again map-obj) #xff))
    (is (equalp (map-from-test-map map-obj) byte-data))))
