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
                 do (typecase next
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

(defmacro define-primitive-property-consumer (specializer (propvals readable next) &body loop-body)
  "Defines a CONSUME-ITEM method for the given item SPECIALIZER.

SPECIALIZER --- The method specializer for the item.
PROPVALS    --- The list that should contain the property values.
READABLE    --- The readable-list being operated on currently.
NEXT        --- Bound to the next (unconsumed) item in the readable-list.
LOOP-BODY   --- The body of the reading loop to execute until the readable is empty.

The return value of the loop-body is discarded. You can use (RETURN) to exit the loop,
for example for when you encounter an item you don't want to read."
  (let ((property (gensym "PROPERTY"))
        (fullp (gensym "FULLP")))
    `(defmethod consume-item ((,property ,specializer) ,readable)
       (values
        (let ((,propvals ()))
          (loop for (,next ,fullp) = (multiple-value-list (peek ,readable))
                while ,fullp
                do (progn ,@loop-body))
          (cons ,property (nreverse ,propvals)))
        NIL))))

(defmacro define-property-function-case (property (args) &body function-clauses)
  "Defines a CONSUME-ITEM method for PROPERTY that has special handling for property-functions.

FUNCTION-CLAUSES ::= function-clause*
FUNCTION-CLAUSE  ::= (function-name form*)

Each function-name is compared by STRING-EQUAL and each clause should return the
property-value to use in its place, or NIL if it should be skipped.

You can use (RETURN) in a clause body to stop reading values altogether."
  (let ((propvals (gensym "PROPVALS"))
        (readable (gensym "READABLE"))
        (next (gensym "NEXT"))
        (result (gensym "RESULT")))
    `(define-primitive-property-consumer (eql ,property) (,propvals ,readable ,next)
       (typecase ,next
         (keyword (return))
         (list
          (let* ((,args (cdr ,next))
                 (,result
                   (cond ,@(loop for (func . forms) in function-clauses
                                 for alternatives = (if (listp func) func (list func))
                                 collect `((or ,@(loop for alt in alternatives
                                                       collect `(string-equal (car ,next) ,(string alt))))
                                           ,@forms)))))
            (if ,result
                (progn (push ,result propvals) (advance ,readable))
                (return))))
         (T (push (consume ,readable) ,propvals))))))

(defmacro define-simple-property-functions (property &rest funcspecs)
  (let ((arglist (gensym "ARGLIST")))
    `(define-property-function-case ,property (,arglist)
       ,@(loop for (name args) in funcspecs
               do (assert (loop for key in '(&key &optional &rest &allow-other-keys)
                                never (find key args)) () "Only required arguments are allowed.")
               collect `(,name (destructuring-bind ,args ,arglist
                                 (format NIL ,(format NIL "~(~a~)(~{~*~~a~^,~})" name args)
                                         ,@(loop for arg in args collect `(resolve ,arg)))))))))
