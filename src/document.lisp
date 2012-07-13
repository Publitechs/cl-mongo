(in-package :cl-mongo)
(use-package :closer-mop )

#|
  Document is a collection of key/value pairs
|#

(defconstant +field-unbound-value+ (gensym))

(defun make-elements (size)
  (make-hash-table :test #'equal :size size))

(defclass document-class (standard-class) ())

(defmethod validate-superclass ((sub document-class) (super standard-class))
  t)

(defclass document-slot-definition (standard-direct-slot-definition)
  ((printable :initform t :initarg :printable :reader printable-slotp)))

(defclass document-effective-slot-definition (standard-effective-slot-definition)
  ((printable :initform t :initarg :printable :reader printable-slotp)))

(defmethod direct-slot-definition-class ((class document-class) &key)
  'document-slot-definition)

(defmethod effective-slot-definition-class ((class document-class) &rest initargs)
  (declare (ignore initargs))
  'document-effective-slot-definition)

(defmethod compute-effective-slot-definition ((class document-class) name direct-slots)
  (let* ((normal-slot  (call-next-method))
         (direct-slot (find name direct-slots :key #'slot-definition-name)))
    ;(print (printable-slotp direct-slot))
    (setf (slot-value normal-slot 'printable) (printable-slotp direct-slot))
    normal-slot))

(defgeneric to-field-name (something))

(defmethod to-field-name ((symbol symbol))
  (to-field-name (symbol-name symbol)))

(defmethod to-field-name ((string string))
  (string-downcase string))

(defmethod to-field-name ((slot-def standard-effective-slot-definition))
  (to-field-name (slot-definition-name slot-def)))
    
(defun set-field-value (document slot new-value)
  (setf (gethash (to-field-name slot) (_elements document)) new-value))

(defun get-field-value (document slot &optional default)
  (gethash (to-field-name slot) (_elements document) default))

(defun field-boundp (document slot)
  (not (eq +field-unbound-value+ (get-field-value document slot +field-unbound-value+))))

(defun unbound-field (document slot)
  (remhash (to-field-name slot) (_elements document))
  (mark-as-changed))

(defun fieldp (slot-def)
  (eq (slot-definition-allocation slot-def) :field))

(defun mark-as-changed (document)
  (setf (slot-value document 'saved) nil))

(defmethod (setf slot-value-using-class) (new-value (class document-class) document slot)
  (case (slot-definition-allocation slot)
    (:field
     (set-field-value document slot new-value)
     (mark-as-changed document))
    (t (call-next-method))))

(defmethod slot-value-using-class ((class document-class) document slot)
  (case (slot-definition-allocation slot)
    (:field
     (get-field-value document slot))
    (t (call-next-method))))

(defmethod slot-boundp-using-class ((class document-class) document slot)
  (case (slot-definition-allocation slot)
    (:field 
     (field-boundp document slot))
    (t (call-next-method))))

(defmethod slot-makunbound-using-class ((class document-class) document slot)
  (case (slot-definition-allocation slot)
    (:field
     (unbound-field document slot ))
    (t (call-next-method))))
  
(defclass document ()
  ((_elements  :initarg :elements :reader _elements :printable nil)
   (_local_id :initarg :local    :reader _local :printable nil)
   (_id       :allocation :field :initarg :_id :reader _id)
   (saved :initarg :saved :reader saved :printable nil))
  (:default-initargs
   :local t
   :saved nil
   :_id (make-bson-oid))
  (:metaclass document-class)
  (:documentation "document
Document class. A document consists of key/value pairs stored in a internal hash table plus 
an internally generated unique id.   
Accessors are : 'elements' which returns the internal hash table;
'_id' which  returns the unique id and '_local_id' which if true means that 
the document was generated by the client (as opposed to having been read from the server)."))

(defgeneric set-saved (document))

(defmethod set-saved ((document document))
  (setf (slot-value document 'saved) t))

(export 'before-save)
(defgeneric before-save (document))

(defmethod before-save ((document document)))

(defun make-document ( &key (oid nil) (size 40))
  "Constructor.  key ':oid' is a user supplied unique id. An internal id will be generated if none 
   is supplied."
  (if oid
      (make-instance 'document :oid oid :local nil :saved t :elements (make-elements size))
      (make-instance 'document :elements (make-elements size))))

(defgeneric add-element ( key value document) 
  ( :documentation "add element with key and value to a document" ) )

(defmethod add-element (key value document)
  document)

(defmethod add-element ( (key string) value (document document) )
  (setf (gethash key (_elements document)) value)
  (call-next-method))


(defgeneric get-element ( key document) 
  ( :documentation "Get an element identified by key from the document. To get the _id use :_id as the key" ) )


(defmethod get-element ( (key string) (document (eql nil) ) )
  (values nil nil))

;;
;;  To get the document id use the keyword :_id
;; 
(defmethod get-element ( (key (eql :_id)) (document document))
  (doc-id document))

(defmethod get-element ( (key (eql :_id)) (docs cons))
  (let ((lst ()))
    (dolist (document docs)
      (push (doc-id document) lst))
    lst))

(defgeneric get-keys (document)
  (:documentation "Gets a list of keys that are in the document"))
(defmethod get-keys ((document document))
  (loop for key being the hash-keys of (_elements document)
     collect key))

(defun get-all-values (key list-of-docs)
  (let ((lst ()))
    (dolist (doc list-of-docs)
      (multiple-value-bind (value exists-p) (gethash key (_elements doc))
	(when exists-p (push value lst))))
    (values lst (not (null lst)))))

(defmethod get-element ( (key-list cons) (document document) ) 
  (let ((iter-doc document)
	(exists-p nil))
    (dolist (key key-list)
      (typecase iter-doc
	(document  (multiple-value-bind (doc doc-exists-p)  (gethash key (_elements iter-doc))
		     (setf iter-doc doc exists-p doc-exists-p)));
	(cons (multiple-value-bind (doc doc-exists-p)  (get-all-values key iter-doc)
		(setf iter-doc doc exists-p doc-exists-p)))
	(t    (setf exists-p nil))))

    (values iter-doc exists-p)))
	     
(defmethod get-element ( (key string) (document document) ) 
  (get-element (split-sequence:SPLIT-SEQUENCE #\. key) document))

(defmethod get-element ( (key string) (document cons))
  (let ((lst ())
	(key-list (split-sequence:SPLIT-SEQUENCE #\. key)))
    (dolist (doc document)
      (multiple-value-bind (doc doc-exists-p)  (get-element key-list doc) 
	(when doc-exists-p (push doc lst))))
    lst))

(defgeneric rm-element (key document) 
  ( :documentation "Remove element identified by key from a document" ) )

(defmethod rm-element ( (key string) (document document) ) 
  (remhash key (_elements document)))

(defun collect-all-elements (key-list document-list) 
  (let ((collector nil)) 
    (dolist (doc document-list)
      (let ((lst nil))
	(dolist (key key-list)
	  (multiple-value-bind (elem exists-p)  (get-element key doc) 
	    (declare (ignore exists-p))
	    (push elem lst)))
	(unless (null lst) (push (nreverse lst) collector ))))
    (nreverse collector )))

(defgeneric get-id (id) )

(defmethod get-id ( (id t) )
    id)

(defmethod get-id ( (id bson-oid) )
    (id id))

(defun doc-id (doc)
  "return the unique document id"
  (get-id (_id doc)))

;;
;; When the to-hash-able finalizer is used, embedded docs/tables in the response aren't converted
;; to hash tables but to documents. When print-hash is used we want to see hash table like output
;; so that's what this tries to do..
;;
(defun print-hash (ht stream &key (max (hash-table-count ht)))
  (labels ((prdocs (docs) 
	     (format stream "~1,1T[")
	     (block print-array
	       (let ((counter 0))
		 (dolist (doc docs)
		   (incf counter)
		   (if (typep doc 'document)
		       (print-hash (_elements doc) stream :max max)
		       (format stream "~A," doc))
		   (when (> counter 100)
		     (progn
		       (format stream "[....](~A elements left)" (- (length docs) 100))
		       (return-from print-array nil))))))
	     (format stream "]~%"))
	   (vpr (v)
	     (cond ( (typep v 'cons)     (prdocs v)        )
		   ( (typep v 'document) (prdocs (list v)) )
		   (  t                  (format stream "~A~%" v)))))
    (format stream "~%~3,8T{~%") 
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat max)
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p 
	      (progn 
		(format stream "~3,8T~A -> " key) 
		(vpr value))))))
    (when (< max (hash-table-count ht)) (format stream "~3,8T[..............]~%"))
    (format stream "~3,8T}~%")))

(defun hash-keys (ht)
  (let ((lst))
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (push key lst)))))
    (nreverse lst)))


;
; suppress the printing of the object id if the objectis locally generated
;

(defmethod describe-object ((docs cons)  (stream  t))
  (dolist (document docs)
    (describe-object document stream)))

(defmethod describe-object ((document document)  (stream  t))
  (format stream "~%{  ~S ~%" (type-of document) ) 
  (unless (slot-boundp  document '_id)       (format stream "  _id not set"))
  (unless (slot-boundp  document '_local_id) (format stream "  _local_id not set"))
  (when (and (slot-boundp  document '_local_id) (slot-boundp  document '_id) )
    (unless (_local document) (format stream "  _id : ~A~%" (_id document))))
  (if (slot-boundp document 'elements)
      (print-hash (_elements document) stream)
      "no elements set..")
  (format stream "}~%")) 

(defmethod print-object.2 ((document document) stream)
  (format stream "<~S> : { ~%" (type-of document) ) 
  (when (and (slot-boundp  document '_local_id) (slot-boundp  document '_id) )
    (unless (_local document) (format stream "~3,8T_id : ~A~%" (_id document))))
  (if (slot-boundp document 'elements)
      (progn
	(format stream "~3,8Telements : ~A" (hash-table-count (_elements document) ))
	(print-hash (_elements document) stream :max 20))
      "no elements set..")
  (format stream "}~%"))

(defmethod print-object.1 ((document document) stream)
  (format stream "<~S> : { ~%" (type-of document) ) 
  (when (and (slot-boundp  document '_local_id) (slot-boundp  document '_id) )
    (unless (_local document) (format stream "~3,8T_id : ~A~%" (_id document))))
  (if (slot-boundp document 'elements)
	(format stream "~3,8Telements : ~A" (hash-table-count (_elements document))))
  (format stream "}~%"))

(defmethod print-object ((document document) stream)
  (let ((document-class (class-of document)))
    (format stream "#<  ~{~%~0,2:@T~S: ~S ~}~%~@T>" 
      (loop for slot-def in (class-slots document-class)
            when (printable-slotp slot-def)
            nconc `(
                      ,(if (fieldp slot-def)
                           (to-field-name slot-def)
                           (slot-definition-name slot-def))
                      ,(if (slot-boundp-using-class document-class document slot-def)
                           (slot-value-using-class document-class document slot-def)
                           "N/A"))))))
    
(defun ht->document (ht) 
  "Convert a hash-table to a document."
  (multiple-value-bind (oid oid-supplied) (gethash "_id" ht)
    (let ((doc (make-document :oid (if oid-supplied oid nil))))
      (when oid-supplied (remhash "_id" ht))
      (with-hash-table-iterator (iterator ht)
	(dotimes (repeat (hash-table-count ht))
	  (multiple-value-bind (exists-p key value) (iterator)
	    (if exists-p (add-element key value doc)))))
      doc)))
;;------------


(defun ht->document.1 (ht doc-class) 
  (multiple-value-bind (oid oid-supplied) (gethash "_id" ht)
      (if oid-supplied
	  (progn 
	    (remhash "_id" ht)
	    (make-instance doc-class :_id oid :local nil :elements ht))
	  (make-instance doc-class :elements ht))))

;;--------------------

(defun mapdoc (fn document)
  (let ((lst ())
	(ht (_elements document)))
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (push (funcall fn key value) lst)))))
    (nreverse lst)))
	       
(defgeneric doc-elements (document) )

(defmethod doc-elements ( (document hash-table) )
  (let ((lst ()))
    (with-hash-table-iterator (iterator document)
      (dotimes (repeat (hash-table-count document))
	(multiple-value-bind (exists-p key value) (iterator)
	  (if exists-p (push key lst)))))
    lst))

(defmethod doc-elements ( (document document) )
  (doc-elements (_elements document)))

;;----------------------------

(defun ht-test (size)
  (let ((ht (make-hash-table :test #'equal)))
    (dotimes (var size)
      (setf (gethash var ht) (format nil "value stored : ~A~%" var)))
    ht))
