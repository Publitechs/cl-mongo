(in-package :cl-mongo)

(declaim (inline string-starts-with-p))
(defun string-starts-with-p (datum str)
  (alexandria:starts-with-subseq datum str))
