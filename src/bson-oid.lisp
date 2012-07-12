(in-package :cl-mongo)

#|
  
|#

(defclass bson-oid ()
  ((id :reader id :initarg :oid))
  (:default-initargs
   :oid (cl-mongo-id:oid)))

(defun make-bson-oid( &key (oid nil oid-supplied-p))
  (if oid-supplied-p
      (make-instance 'bson-oid :oid oid)
      (make-instance 'bson-oid)))

(defmethod print-object ((bson-oid bson-oid) stream)
  (format stream "_id(~a)"
	  (if (slot-boundp bson-oid 'id) 
	      (cl-mongo-id:oid-str (id bson-oid))
	      "no id set..")))

(defgeneric _id (bson-oid) 
  (:documentation 
   "return a 12 byte array, irrespective of the internals of bson-oid, which may have a larger  array")) 

(defmethod _id ((bson-oid bson-oid))
  (id bson-oid))

(defmethod _id ((bson-oid-str string))
  (make-bson-oid :oid (cl-mongo-id:oid (subseq bson-oid-str 4 28))))

