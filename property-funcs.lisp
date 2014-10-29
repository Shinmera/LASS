#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *property-functions* (make-hash-table :test 'equalp))

(defun property-function (name)
  (gethash (string name) *property-functions*))

(defun (setf property-function) (function name)
  (setf (gethash (string name) *property-functions*)
        function))

(defmacro define-property-function (name args &body body)
  `(setf (property-function ,(string name))
         #'(lambda ,args ,@body)))

(defmacro define-simple-property-function (name args)
  `(define-property-function ,name ,args
     (format NIL ,(format NIL "~(~a~)(~{~*~~a~^,~})" name args)
             ,@(loop for arg in args collect `(resolve ,arg)))))

(defun resolve-function (function &rest args)
  (let ((resolver (property-function function)))
    (if resolver
        (apply resolver args)
        (format NIL "~(~a~)(~{~a~^,~})" function args))))

(defmethod resolve ((thing list))
  (apply #'resolve-function (car thing) (cdr thing)))

;; We redefine the method here in order to delegate to property functions
;; if they do actually exist.
(defmethod consume-item ((property symbol) readable-list)
    (if (keywordp property)
        (values
         (let ((propvals ()))
           (loop for (next fullp) = (multiple-value-list (peek readable-list))
                 while fullp
                 do (etypecase next
                      (keyword (return))
                      (list
                       (let ((resolver (property-function (car next))))
                         (if resolver
                             (push (consume readable-list) propvals)
                             (return))))
                      (T (push (consume readable-list) propvals))))
           (cons property (nreverse propvals)))
         NIL)
        (call-next-method)))
