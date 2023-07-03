(in-package #:org.tymoonnext.lass)

(defclass readable-list ()
  ((inner :initarg :list :initform () :accessor inner)))

(defun make-readable-list (&optional list)
  (make-instance 'readable-list :list list))

(defmacro with-empty-check ((lis) &body body)
  `(if (null (inner ,lis))
       (values NIL NIL)
       (values (progn ,@body) T)))

(defun empty-p (readable-list)
  (null (inner readable-list)))

(defun consume (readable-list)
  (with-empty-check (readable-list)
    (pop (inner readable-list))))

(defun peek (readable-list)
  (with-empty-check (readable-list)
    (first (inner readable-list))))

(defun advance (readable-list)
  (with-empty-check (readable-list)
    (setf (inner readable-list)
          (cdr (inner readable-list)))))

(defun pushback (item readable-list)
  (push item (inner readable-list)))
