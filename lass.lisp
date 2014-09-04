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
  (list (list :attribute (format NIL "@charset ~a" (resolve charset)))))

(define-special-block document (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@document~{ ~a~^,~}" (compile-selector selector)))
                      inner)))))

(define-special-block font-face (&rest body)
  (compile-block "@font-face" body))

(define-special-block import (url &rest media-queries)
  (list (list :attribute (format NIL "@import ~a~{ ~a~}"
                                 (resolve url)
                                 (mapcar #'resolve media-queries)))))

(define-special-block keyframes (identifier &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@keyframes ~a" (resolve identifier)))
                      inner)))))

(define-special-block media (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (cons :block
                (cons (list (format NIL "@media~{ ~a~^,~}" (compile-selector selector)))
                      inner)))))

(define-special-block namespace (prefix/namespace &optional namespace)
  (list (list :attribute (format NIL "@namespace ~a~@[ ~a~]"
                                 (resolve prefix/namespace)
                                 (when namespace (resolve namespace))))))

(define-special-block page (pseudo-class &rest body)
  (compile-block (format NIL "@page ~a"
                         (if (keywordp pseudo-class)
                             (format NIL ":~a" (string-downcase pseudo-class))
                             (resolve pseudo-class))) body))

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

(define-special-attribute font-family (&rest faces)
  (list (list :attribute "font-family" (format NIL "~{~a~^, ~}" (mapcar #'resolve faces)))))

(defmacro define-attr-comparator (comp)
  `(define-special-selector ,comp (attr value)
     (loop with out = ()
           with values = (compile-selector value)
           for attr in (compile-selector attr)
           do (loop for value in values
                    do (push (format NIL ,"[~a~a~a]" attr ,(string comp) value) out))
           finally (return (nreverse out)))))

(define-attr-comparator =)
(define-attr-comparator ~=)
(define-attr-comparator *=)
(define-attr-comparator $=)
(define-attr-comparator ^=)
(define-attr-comparator /=)

(defmacro define-single-arg-selector (name)
  `(define-special-selector ,name (arg)
     (loop for arg in (compile-selector arg)
           collect (format NIL ":~a(~a)" ,(string-downcase name) arg))))

(define-single-arg-selector dir)
(define-single-arg-selector lang)
(define-single-arg-selector not)
(define-single-arg-selector nth-child)
(define-single-arg-selector nth-last-child)
(define-single-arg-selector nth-last-of-type)
(define-single-arg-selector nth-of-type)

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
