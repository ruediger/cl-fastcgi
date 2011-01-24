; Copyright (C) 2006 by Rüdiger Sonderfeld <ruediger@c-plusplus.de>

(in-package :fcgi)

(defun compose-2-to (sym count)
  (values
   `(setf (ldb (byte 8 8) (,sym obj)) (aref byte-data ,count)

	  (ldb (byte 8 0) (,sym obj)) (aref byte-data ,(incf count)))
   (1+ count)))

(defun compose-2-from (sym count)
  (values
   `(setf (aref byte-data ,count) (ldb (byte 8 8) (,sym obj))
	  (aref byte-data ,(incf count)) (ldb (byte 8 0) (,sym obj)))
   (1+ count)))

(defun compose-4-to (sym count)
  (values
   `(setf (ldb (byte 8 24) (,sym obj)) (aref byte-data ,count)
	  (ldb (byte 8 16) (,sym obj)) (aref byte-data ,(incf count))
	  (ldb (byte 8  8) (,sym obj)) (aref byte-data ,(incf count))
	  (ldb (byte 8  0) (,sym obj)) (aref byte-data ,(incf count)))
   (1+ count)))

(defun compose-4-from (sym count)
  (values
   `(setf (aref byte-data ,count) (ldb (byte 8 24) (,sym obj))
	  (aref byte-data ,(incf count)) (ldb (byte 8 16) (,sym obj))
	  (aref byte-data ,(incf count)) (ldb (byte 8  8) (,sym obj))
	  (aref byte-data ,(incf count)) (ldb (byte 8  0) (,sym obj)))
   (1+ count)))

(defun simple-set-to (sym count)
  (values
   `(setf (,sym obj) (aref byte-data ,count))
   (1+ count)))

(defun simple-set-from (sym count)
  (values
   `(setf (aref byte-data ,count) (,sym obj))
   (1+ count)))

(defmacro binary-mapper (name &rest mapping)
  "Expands to the definition of a class and functions for mapping an object
   of that class to binary data and vice-versa.

   `name' is the name of the class and `mapping' are the slots for the class.
   An element of `mapping' is either a symbol representing the slot name
   or a list with the symbol as first parameter and `compose-2' or
   `compose-4' as second parameter.

   `compose-2' means that the slot is composed from 2 bytes and `compose-4'
   means that the slot is composed from 4 bytes. Other slots are 1 byte only.

   Each slot is initialized with 0 and has an accessor and initarg with the same
   name as the slot.

   The macro creates the functions map-to-`name' and map-from-`name'.
   map-to-`name' takes a (vector (unsigned-byte 8)) as argument which is
   converted to an object of class `name'. map-from-`name' takes an object of
   class `name' as argument and converts it to an (vector (unsigned-byte 8)).

   The constant +`name'-size+ is the size of the corresponding byte vector.
  "
  (let ((type-in 1) (to-in 2) (from-in 3) (size-in 4)
	(type-mapper '(#(compose-2
			 '(unsigned-byte 16)
			 compose-2-to
			 compose-2-from
			 2)
		       #(compose-4
			 '(unsigned-byte 32)
			 compose-4-to
			 compose-4-from
			 4))))
    `(progn
       (defclass ,name ()
	 ,(mapcar #'(lambda (n)
		      (if (listp n)
			  (list (first n) :initform 0 :accessor (first n)
				:initarg (intern (symbol-name (first n))
						 :keyword)
				:type (aref (find-if #'(lambda (te)
							 (eq (aref te 0)
							     (second n)))
						     type-mapper)
					    type-in))
			  (list n :initform 0 :accessor n
				:initarg (intern (symbol-name n) :keyword)
				:type '(unsigned-byte 8))))
		  mapping))
       (defun ,(intern (format nil "MAP-TO-~A" name)) (byte-data)
	 (let ((obj (make-instance ',name)))
	   ,@(let ((count 0))
		  (mapcar #'(lambda (n)
			      (if (listp n)
				  (multiple-value-bind (ret ptr)
				      (funcall
				       (aref (find-if #'(lambda (te)
							  (eq (aref te 0)
							      (second n)))
						      type-mapper)
					     to-in)
				       (first n) count)
				    (setf count ptr)
				    ret)
				  (multiple-value-bind (ret ptr)
				      (simple-set-to n count)
				    (setf count ptr)
				    ret)))
			  mapping))
	   obj))
       (defun ,(intern (format nil "MAP-FROM-~A" name)) (obj)
	 (let* ((bytes ,(loop for i in mapping
			      sum (if (listp i)
				      (aref
				       (find-if #'(lambda (te)
						    (eq (aref te 0)
							(second i)))
						type-mapper)
				       size-in)
				      1) into size
			      finally (return size)))
		(byte-data (make-array bytes :element-type '(unsigned-byte 8))))
	   ,@(let ((count 0))
		  (mapcar #'(lambda (n)
			      (if (listp n)
				  (multiple-value-bind (ret ptr)
				      (funcall
				       (aref (find-if #'(lambda (te)
							  (eq (aref te 0)
							      (second n)))
						      type-mapper)
					     from-in)
				       (first n) count)
				    (setf count ptr)
				    ret)
				  (multiple-value-bind (ret ptr)
				      (simple-set-from n count)
				    (setf count ptr)
				    ret)))
			  mapping))
	   byte-data))
       (defconstant ,(intern (format nil "+~A-SIZE+" name))
			      ,(loop for i in mapping
			      sum (if (listp i)
				      (aref
				       (find-if #'(lambda (te)
						    (eq (aref te 0)
							(second i)))
						type-mapper)
				       size-in)
				      1) into size
			      finally (return size))))))
