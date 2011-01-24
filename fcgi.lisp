; Copyright (C) 2006 by Rüdiger Sonderfeld <ruediger@c-plusplus.de>

(in-package :fcgi)

(define-condition not-enough-bytes-read (error)
  ((bytes-read :initarg :bytes-read :reader bytes-read)
   (bytes-required :initarg :bytes-required :reader bytes-required)))
; Should rather loop-read until enough bytes read!

(define-condition begin-request-on-active-request (error)
  ((request-id :initarg :request-id :reader request-id)))

(define-condition fastcgi-version-not-supported (error)
  ((version :initarg :version :reader version)))

(define-condition unknown-type (error)
  ((conn-type :initarg :conn-type :reader conn-type)))

(define-condition escape-sequence-unknown (error)
  ((seq :initarg :seq :reader seq)))

(defclass fcgi-conn ()
  ((active :initform nil :initarg :active :accessor active)
   (request-id :initform nil :initarg :request-id :accessor request-id)
   (connection-fd :initarg :connection-fd :initform nil :accessor connection-fd)
   (role :initarg :role :accessor role)
   (env :initarg :env :initform nil :accessor environment)
   (stdin :initarg :stdin :accessor stdin)
   (keep-conn :initarg :keep-conn :initform nil :accessor keep-conn)))

(defclass fcgi-main ()
  ((socket :accessor socket)
   (request-ids :initform (make-array #.+fcgi-max-connections+
				      :initial-element (make-instance
							'fcgi-conn)
				      :element-type 'fcgi-conn) :type 'array
				      :accessor request-ids)))

(defmethod init-local-socket ((fcgi fcgi-main) socket-path
			      &optional (backlog 3))
  "Connects an fcgi-main object to a LOCAL-SOCKET (UNIX-SOCKET)
   `socket-path' is the path to the socket"
  (with-accessors ((sock socket)) fcgi
    (setf sock (make-instance 'sb-bsd-sockets:local-socket :type :stream))
    (sb-bsd-sockets:socket-bind sock socket-path)
    (sb-bsd-sockets:socket-listen sock backlog)))

; UNTESTED
(defmethod init-remote-socket ((fcgi fcgi-main) port &optional (hostname :any)
			       (backlog 3))
  "Connects an fcgi-main object to a REMOTE-SOCKET
   `port' is the local port
   `hostname' is an optional hostname"
  (with-accessors ((sock socket)) fcgi
    (setf sock (make-instance 'sb-bsd-sockets:socket :type :stream
			      :protocol :tcp))
    (sb-bsd-sockets:socket-bind sock (resolve-hostname hostname) port)
    (sb-bsd-sockets:socket-listen sock backlog)))

(defun read-fcgi-header (conn)
  "reads byte-data from conn and maps it to a fcgi-header"
  (let ((byte-data
	 (make-array +fcgi-header-size+ :element-type '(unsigned-byte 8))))
    (multiple-value-bind (buffer length addr)
	(sb-bsd-sockets:socket-receive conn byte-data nil)
      (declare (ignore addr))
      (if (= length +fcgi-header-size+)
	  (map-to-fcgi-header buffer)
	  (error 'not-enough-bytes-read :bytes-read length
		 :bytes-required +fcgi-header-size+)))))

(defmethod begin-request ((fcgi fcgi-main) header conn)
  (let ((byte-data (make-array (+ (content-length header)
				  (padding-length header))
			       :element-type '(unsigned-byte 8))))
    (multiple-value-bind (buffer length addr)
	(sb-bsd-sockets:socket-receive conn byte-data nil)
      (declare (ignore addr)) ; TODO: Check addr for WEB_SERVER_ADDRS (see Spec)
      (if (>= length +fcgi-begin-request-body-size+)
	  (let ((begin-req-body (map-to-fcgi-begin-request-body buffer)))
	    (when (active (aref (request-ids fcgi) (request-id header)))
	      (error 'begin-request-on-active-request
		     :request-id (request-id header)))
	      (setf (aref (request-ids fcgi) (request-id header))
		    (make-instance 'fcgi-conn :active t
				   :request-id (request-id header)
				   :connection-fd conn
				   :role (role begin-req-body)
				   :keep-conn (logand (flags begin-req-body)
						      +fcgi-keep-conn+))))
	  (error 'not-enough-bytes-read :bytes-read length
		 :bytes-required +fcgi-begin-request-body-size+)))))

; UNTESTED
(defmethod unknown-type ((fcgi fcgi-main) header conn)
  "Sends an UNKNOWN_TYPE-Message to the httpd"
  (let ((ret-header (make-instance 'fcgi-header
				   :version +fcgi-version+
				   :conn-type +fcgi-unknown-type+
				   :request-id +fcgi-null-request-id+
				   :content-length +fcgi-unknown-type-body-size+
				   :padding-length 0))
	(body (make-instance 'fcgi-unknown-type-body
			     :conn-type (conn-type header))))
    (sb-bsd-sockets:socket-send conn
				(concatenate '(vector (unsigned-byte 8))
					     (map-from-fcgi-header ret-header)
					     (map-from-fcgi-unknown-type-body
					      body))
				nil)))

; UNTESTED!
(defmethod get-values-result ((fcgi fcgi-main) header conn)
  "Interpretes a GET_VALUES_RESULT-Message and sends an answer to the httpd"
  (let ((byte-data (make-array (+ (content-length header)
				  (padding-length header))
			       :element-type '(unsigned-byte 8)
			       :fill-pointer (content-length header))))
    (multiple-value-bind (buffer length addr)
	(sb-bsd-sockets:socket-receive conn byte-data nil)
      (declare (ignore addr) (ignore buffer))
      (when (< length (content-length header))
	(error 'not-enough-bytes-read :bytes-read length
	       :bytes-required (content-length header)))
      (let* ((body (reduce 
		    #'(lambda (&optional a b)
			(concatenate '(vector (unsigned-byte 8))
				     a b))
		    (read-name-value-pairs byte-data
					   #'(lambda (pair)
					       (write-name-value-pair
						(case (car pair)
						  ("FCGI_MAX_CONNS"
       (cons (str-to-vector-ub8 "FCGI_MAX_CONNS")
	     (str-to-vector-ub8 (to-string #.+fcgi-max-connections+))))
						  ("FCGI_MAX_REQS"
       (cons (str-to-vector-ub8 "FCGI_MAX_REQS")
	    (str-to-vector-ub8 "1")))
						  ("FCGI_MPXS_CONN"
       (cons (str-to-vector-ub8 "FCGI_MPXS_CONN")
	     (str-to-vector-ub8 "0")))))))))
	     (ret-header (make-instance 'fcgi-header
					:version +fcgi-version+
					:conn-type +fcgi-get-values-result+
					:request-id +fcgi-null-request-id+
					:content-length (length body)
					:padding-length 0)))
	(sb-bsd-sockets:socket-send conn
				    (concatenate '(vector (unsigned-byte 8))
						 (map-from-fcgi-header
						  ret-header)
						 body)
				    nil)))))

(defun read-stream (header conn)
  "Reads stream data from conn."
  (loop
      with hdr = header
      with data = nil
      until (= (content-length hdr) 0)
      do
      (progn
     ; read body
	(with-slots (content-length) hdr
	  (let ((byte-data (make-array (+ content-length
					  (padding-length hdr))
				       :element-type '(unsigned-byte 8)
				       :fill-pointer t)))
	    (multiple-value-bind (buffer length addr)
		(sb-bsd-sockets:socket-receive conn byte-data nil)
	      (declare (ignore addr) (ignore buffer))
	      (when (< length content-length)
		(error 'not-enough-bytes-read :bytes-read length
		       :bytes-required content-length)))
	    (setf (fill-pointer byte-data) content-length)
	    (setf data (concatenate 'vector data byte-data))))
    ; read next package header  
	(setf hdr (read-fcgi-header conn)))
      finally (return data)))

(defmethod read-params ((fcgi fcgi-main) header conn)
  "Reads a stream of FCGI_PARAMS packages and initializes the fcgi-conn
   environment"
  (assert (active (aref (request-ids fcgi) (request-id header))))
  (read-name-value-pairs
   (read-stream header conn)
   #'(lambda (pair)
       (push pair (environment
		   (aref (request-ids fcgi) (request-id header)))))))

(defmethod read-stdin ((fcgi fcgi-main) header conn)
  "Reads a stream of FCGI_STDIN packages and sets the fcgi-conn stdin slot"
  (assert (active (aref (request-ids fcgi) (request-id header))))
  (setf (stdin (aref (request-ids fcgi) (request-id header)))
	(read-stream header conn)))

(defmethod read-package ((fcgi fcgi-main) conn)
  "Reads a package from conn and handles it"
  (let ((hdr (read-fcgi-header conn)))
    (unless (= (version hdr) +fcgi-version+)
      (error 'fastcgi-version-not-supported :version (version hdr)))
    (values (conn-type hdr)
	    (case (conn-type hdr)
	      (#.+fcgi-begin-request+ (begin-request fcgi hdr conn))
	      (#.+fcgi-abort-request+ ) ; TODO
	      (#.+fcgi-params+ (read-params fcgi hdr conn))
	      ((#.+fcgi-stdin+ #.+fcgi-data+) (read-stdin fcgi hdr conn))
	      (#.+fcgi-get-values+ (get-values-result fcgi hdr conn))
	      (otherwise 
	       (if (= (request-id hdr) +fcgi-null-request-id+)
		   (unknown-type fcgi hdr conn)
		   (error 'unknown-type :conn-type (conn-type hdr))))))))

(defmethod accept ((fcgi fcgi-main))
  "Handles new Connections"
  (loop do (let ((conn (sb-bsd-sockets:socket-accept (socket fcgi))))
	     (setf (non-blocking-mode conn) t)
	     (multiple-value-bind (pkg-type ret)
		 (read-package fcgi conn)
	       (when (= pkg-type +fcgi-begin-request+)
		 (return ret))))))

(defmethod setup-environment ((fcgi fcgi-main) (conn fcgi-conn))
  "sets up the connection environemnt"
  (with-slots (connection-fd) conn
    (loop do
	 (multiple-value-bind (pkg-type ret)
	     (read-package fcgi connection-fd)
	   (declare (ignore ret))
	   (when (= pkg-type #.+fcgi-params+)
	     (loop-finish))))
    (read-package fcgi connection-fd)))  ; read stdin data if possible

(defmethod end-request ((conn fcgi-conn) app-status
			&optional (protocol-status #.+fcgi-request-complete+))
  "Signals the httpd that the request was finished.

   The value of app-status depends on the role type but is usually the
   unix-style program return value"
  (let ((header (make-instance 'fcgi-header
			       :version +fcgi-version+
			       :conn-type +fcgi-end-request+
			       :request-id (request-id conn)
			       :content-length +fcgi-end-request-body-size+
			       :padding-length 0))
	(body (make-instance 'fcgi-end-request-body
			     :app-status app-status
			     :protocol-status protocol-status)))
    (sb-bsd-sockets:socket-send (connection-fd conn)
				(concatenate '(vector (unsigned-byte 8))
					     (map-from-fcgi-header header)
					     (map-from-fcgi-end-request-body
					      body))
				nil))

  (unless (keep-conn conn)
    (sb-bsd-sockets:socket-close (connection-fd conn)))
  (setf (environment conn) nil)
  (setf (stdin conn) nil)
  (setf (active conn) nil))

(defmethod fcgi-write ((conn fcgi-conn) data type)
  "Sends data to the stream which is specified in `type'.

   `data' can be either of type string or (vector (unsigned-byte 8)).

   This funciton is library intern. Library Users should use write-stdout or
   write-stderr instead!"
  (when (stringp data)
    (setf data (map '(vector (unsigned-byte 8)) #'(lambda (c) (char-code c))
		    data)))

  (loop
     with length = (length data)
     with pos = 0
     with header = (make-instance 'fcgi-header
				  :version #.+fcgi-version+
				  :conn-type type
				  :request-id (request-id conn)
				  :padding-length 0)
     do (progn 
	  (setf (content-length header)
		(if (> (- length pos) #xffff)
		    #xffff
		    length))
	  (sb-bsd-sockets:socket-send (connection-fd conn)
				      (concatenate '(vector (unsigned-byte 8))
						   (map-from-fcgi-header header)
				 (subseq data pos (content-length header)))
				      nil)
	  (incf pos (content-length header)))
     until (= pos length)
     finally (progn
	       (setf (content-length header) 0)
	       (sb-bsd-sockets:socket-send (connection-fd conn)
					   (map-from-fcgi-header header)
					   nil))))

(defmethod write-stdout ((conn fcgi-conn) data)
  "Writes data to STDOUT (to the client)"
  (fcgi-write conn data #.+fcgi-stdout+))

(defmethod write-stderr ((conn fcgi-conn) data)
  "Writes data to STDERR (to the error-log)"
  (fcgi-write conn data #.+fcgi-stderr+))

(defun expand-escape-sequences (string)
  "converts C-style escape sequence \r and \n to (code-char 13) and
                                                 (code-char 12)

   WARNING: This function could be Buggy on CLisp/Win32 due to the fact that
            (code-char 13) is handled as #\Newline"
  (loop
     with tmp-str = string
     with pos = 0
     while (setf pos (search "\\" tmp-str :start2 (1+ pos)))
     do (progn
	  (setf tmp-str (string-remove tmp-str pos 1))
	  (replace tmp-str
		   (string
		    (case (aref tmp-str pos)
		      (#\n (code-char 10))
		      (#\r (code-char 13))
		      (#\\ #\\)
		      (otherwise (error 'escape-sequence-unkwon
					:seq (aref tmp-str pos)))))
		   :start1 pos))
     finally (return tmp-str)))
       
(defun ls-reader (socket-path)
  "FOR TESTING PURPOSE ONLY"
  (let ((fcgi (make-instance 'fcgi-main)))
    (init-local-socket fcgi socket-path)
    (let ((conn (accept fcgi)))
      (setup-environment fcgi conn)
      (let ((env (environment conn))
	    (stdin (stdin conn)))
	(write-stdout conn
		      (expand-escape-sequences
  "Content-type: text/html\\r\\n\\r\\n<title>ho</title><p>Hello!</p>\\r\\n"))
	(write-stderr conn "Hello Err-Log!")
	(end-request conn 0)
	(values stdin env)))))

; Keep updated: xristos <ccalca@essex.ac.uk>
