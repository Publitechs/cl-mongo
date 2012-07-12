(use-package :cl-mongo)

(defclass Model (document)
  ((saved :initarg :saved :initform nil :accessor saved))
  (:metaclass document-class))

(defmacro defmodel (name &rest (slots))
  `(progn
     (defclass name (Model)
       ,(to-defclass-slots name slots)
       (metaclass :document-class))
     ,@(create-model-helpers name)
     ,@(create-fieldhelpers name slots)))

(defun to-defclass-slots (model-name model-slots)
  (loop for (allocation name) in model-slots
        collect `(,name
                   ,@(when (eq allocation :field)
                          (:allocation :field))
                   :reader (intern #?"SET-{name}")
                   :initarg (intern name :keyword)
                   :writer name)))

(defun create-model-helpers (model-name)
  `((defun ,(intern #?"FIND-{model-name}S")
        (&optional (connection (get-mongo-connection)) &key skip limit))
    (defun ,(intern #?"FIND-{model-name}")
        (&optional connection))
    (defun ,(intern #?"COUNT-{model-name}S")
        (selector &optional connection))
    (defmethod delete ((worker Worker) &key connection &key all))
    (defmethod delete-first ((worker Worker) &key connection &key all))
    (defmethod save ((worker Worker) &key connection))
    (defmethod update ((worker Worker) &key connection))
    (defmethod replace-first ((worker Worker) &key connection))))

(defun create-field-helpers (model-name model-slots)
  (loop for (allocation name) in model-slots
        when (eq allocation :field)
        collect `(progn
                   (defun ,(intern #?"COUNT-{model-name}-BY-{name}")
                       (,name &key connection))
                   (defun ,(intern #?"FIND-{model-name}-BY-{name}")
                       (,name &key connection))
                   (defun ,(intern #?"DELETE-{model-name}-BY-{name}")
                       (,name &key connection skip limit))
                   (defun ,(intern #?"FIND-{model-name}S-BY-{name}")
                       (,name &key connection skip limit))
                   (defun ,(intern #?"DELETE-{model-name}S-BY-{name}")
                       (,name &key connection skip limit)))))

(defmodel Worker
  (:slot password)
  (:field login)
  (:field name)
  (:field password-hash)
  (:field password-salt)
  (:field gender))

(defmethod before-save ((worker Worker))
  (when (password worker)
    (with-slots (password password-salt password-hash) worker
      (setf password-salt (sha (time-now (rand))))
      (setf password-hash (encrypt-password password password-salt)))))

(defun authenticate-worker (login password)
  (let ((worker (find-worker-by-login login)))
    (when worker
      (eq (password-hash worker) (encrypt-password
                                   password
                                   (password-salt worker))))))
                 
(defun encrypt-password (password salt)
  (sha password salt))
