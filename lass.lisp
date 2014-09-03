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

(define-special-block charset (charset)
  (list (list :attribute (format NIL "@charset ~a" (selective-downcase charset)))))

(define-special-block document (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@document~{ ~a~^,~}" (compile-selector selector)))
                      inner)))))

(define-special-block font-face (&rest body)
  (compile-block "@font-face" body))

(define-special-block import (url &rest media-queries)
  (list (list :attribute (format NIL "@import ~a~{ ~a~}"
                                 (selective-downcase url)
                                 (mapcar #'selective-downcase media-queries)))))

(define-special-block keyframes (identifier &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@keyframes ~a" (selective-downcase identifier)))
                      inner)))))

(define-special-block media (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@media~{ ~a~^,~}" (compile-selector selector)))
                      inner)))))

(define-special-block namespace (prefix/namespace &optional namespace)
  (list (list :attribute (format NIL "@namespace ~a~@[ ~a~]"
                                 (selective-downcase prefix/namespace)
                                 (when namespace (selective-downcase namespace))))))

(define-special-block page (pseudo-class &rest body)
  (compile-block (format NIL "@page ~a"
                         (if (keywordp pseudo-class)
                             (format NIL ":~a" (string-downcase pseudo-class))
                             (selective-downcase pseudo-class))) body))

(define-special-block supports (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@supports~{ ~a~^,~}" (compile-selector selector)))
                      inner)))))

(defmacro bind-vars (bindings &body body)
  `(let ((*vars* (let ((table (make-hash-table)))
                   (maphash #'(lambda (k v) (setf (gethash k table) v)) *vars*)
                   (loop for (k v) in ,bindings
                         do (setf (gethash k table) v))
                   table)))
     ,@body))

(define-special-block let (bindings &rest body)
  (bind-vars bindings
    (apply #'compile-sheet body)))

