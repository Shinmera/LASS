#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defmacro define-special-block (name args &body body)
  (let ((argsym (gensym "ARGS")))
    `(defmethod compile-block ((,(gensym "TYPE") (eql ,(intern (string name) "KEYWORD"))) ,argsym)
       (destructuring-bind ,args ,argsym
         ,@body))))

(defmacro define-special-attribute (name args &body body)
  (let ((argsym (gensym "ARGS")))
    `(defmethod compile-attribute ((,(gensym "ATTR") (eql ,(intern (string name) "KEYWORD"))) ,argsym)
       (destructuring-bind ,args ,argsym
         ,@body))))

(defmacro define-special-selector (name args &body body)
  (let ((argsym (gensym "ARGS")))
    `(defmethod compile-constraint ((,(gensym "FUNC") (eql ,(intern (string name) "KEYWORD"))) ,argsym)
       (destructuring-bind ,args ,argsym
         ,@body))))

(defun generate (in &key (out (merge-pathnames (make-pathname :type "css") in)) (pretty NIL) (if-exists :supersede))
  (let ((eof (gensym "EOF")))
    (with-open-file (outstream out :direction :output :if-exists if-exists)
      (write-sheet
       (apply #'compile-sheet
              (with-open-file (instream in :direction :input)
                (loop for read = (read instream NIL eof)
                      until (eql read eof)
                      collect read)))
       :stream outstream :pretty pretty))
    out))
