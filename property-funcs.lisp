#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *property-functions* (make-hash-table :test 'equalp))

(defun property-function (name)
  "Returns a function to process a property function of NAME, if any."
  (gethash (string name) *property-functions*))

(defun (setf property-function) (function name)
  "Sets FUNCTION as the new processor for the property function NAME."
  (setf (gethash (string name) *property-functions*)
        function))

(defun remove-property-function (name)
  "Removes the property function NAME."
  (remhash (string name) *property-functions*))

(defmacro define-property-function (name args &body body)
  "Define a new property function NAME, accepting ARGS. 
The body should return a value to use directly, if possible a string.
The results of a property-function should not be RESOVLEd.

Property functions are function calls that occur as (part of) the
value of a property. Due to ambiguity issues with a general sub-block,
property functions need to be explicitly defined and may completely
differ depending on the property. Property functions defined with this
are only the defaults available for all properties. If you want to
minimise collision probability or avoid an illegal function for a
certain property, you should define a direct method on CONSUME-ITEM
to handle the reading of the property values manually."
  `(setf (property-function ,(string name))
         #'(lambda ,args ,@body)))

(defmacro define-simple-property-function (name args)
  "Defines a property function that returns name(arg1,arg2...).
Only required arguments are allowed."
  (assert (loop for key in '(&key &optional &rest &allow-other-keys)
                never (find key args)) () "Only required arguments are allowed.")
  `(define-property-function ,name ,args
     (format NIL ,(format NIL "~(~a~)(~{~*~~a~^,~})" name args)
             ,@(loop for arg in args collect `(resolve ,arg)))))

(defun resolve-function (function &rest args)
  "Turns the FUNCTION with its ARGS into a properly usable property value."
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
