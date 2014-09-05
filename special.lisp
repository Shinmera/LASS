#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

;;; BLOCKS

(define-special-block charset (charset)
  (list (list :attribute (format NIL "@charset ~a" (resolve charset)))))

(define-special-block document (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (make-block
           (list (format NIL "@document~{ ~a~^,~}" (compile-selector selector)))
           inner))))

(define-special-block font-face (&rest body)
  (compile-block "@font-face" body))

(define-special-block import (url &rest media-queries)
  (list (make-attribute
         (format NIL "@import ~a~{ ~a~}"
                 (resolve url)
                 (mapcar #'resolve media-queries)))))

(define-special-block keyframes (identifier &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (make-block
           (list (format NIL "@keyframes ~a" (resolve identifier)))
           inner))))

(define-special-block media (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (make-block
           (list (format NIL "@media~{ ~a~^,~}" (compile-selector selector)))
           inner))))

(define-special-block namespace (prefix/namespace &optional namespace)
  (list (make-attribute
         (format NIL "@namespace ~a~@[ ~a~]"
                 (resolve prefix/namespace)
                 (when namespace (resolve namespace))))))

(define-special-block page (pseudo-class &rest body)
  (compile-block (format NIL "@page ~a"
                         (if (keywordp pseudo-class)
                             (format NIL ":~a" (string-downcase pseudo-class))
                             (resolve pseudo-class))) body))

(define-special-block supports (selector &rest body)
  (let ((inner (apply #'compile-sheet body)))
    (list (make-block
           (list (format NIL "@supports~{ ~a~^,~}" (compile-selector selector)))
           inner))))

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

;;; SELECTORS

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

;;; ATTRIBUTES

(define-special-attribute font-family (&rest faces)
  (list (make-attribute "font-family" (format NIL "~{~a~^, ~}" (mapcar #'resolve faces)))))

(defmacro define-browser-attribute (name args &body browser-options)
  `(define-special-attribute ,name ,args
     (list ,@(loop for (browser prefix) in '((:moz "-moz-")
                                             (:o "-o-")
                                             (:webkit "-webkit-")
                                             (:ms "-ms-")
                                             (:w3c ""))
                   for body = (or (assoc browser browser-options)
                                  (assoc :default browser-options))
                   collect `(let ((,(caadr body) ,(format NIL "~a~a" prefix (string-downcase name))))
                              ,@(cddr body))))))

(indent:define-indentation define-browser-attribute (4 6 &rest (&whole 2 0 4 2)))

(define-browser-attribute linear-gradient (direction &rest colors)
  (:default (attribute)
    (make-attribute "background" (format NIL "~a(~a~{, ~a ~a~})"
                                         attribute (resolve direction) (mapcar #'resolve colors)))))

(define-browser-attribute radial-gradient (shape size position &rest colors)
  (:default (attribute)
    (make-attribute "background" (format NIL "~a(~a ~a at ~a~{, ~a ~a~})"
                                         attribute (resolve shape) (resolve size) (resolve position) (mapcar #'resolve colors)))))

(define-browser-attribute repeating-radial-gradient (shape size position &rest colors)
  (:default (attribute)
    (make-attribute "background" (format NIL "~a(~a ~a at ~a~{, ~a ~a~})"
                                         attribute (resolve shape) (resolve size) (resolve position) (resolve colors)))))

(define-browser-attribute transform (value/function &rest function-args)
  (:default (attribute)
    (make-attribute attribute (format NIL "~a~@[(~{~a~^, ~})~]"
                                      (resolve value/function) (resolve function-args)))))

(define-browser-attribute transform-origin (value/x &optional y z)
  (:default (attribute)
    (make-attribute attribute (format NIL "~a~@[ ~a~]~@[ ~a~]"
                                      (resolve value/x) (resolve y) (resolve z)))))

(define-browser-attribute transform-style (style)
  (:default (attribute)
    (make-attribute attribute (resolve style))))

(define-browser-attribute transition (value/property &optional duration timing-function delay)
  (:default (attribute)
    (make-attribute attribute (format NIL "~a~@[ ~a~]~@[ ~a~]~@[ ~a~]"
                                      (resolve value/property) (resolve duration) (resolve timing-function) (resolve delay)))))

(define-browser-attribute transition-delay (value)
  (:default (attribute)
    (make-attribute attribute (resolve value))))

(define-browser-attribute transition-duration (value)
  (:default (attribute)
    (make-attribute attribute (resolve value))))

(define-browser-attribute transition-property (value)
  (:default (attribute)
    (make-attribute attribute (resolve value))))

(define-browser-attribute transition-timing-function (value/function &rest function-args)
  (:default (attribute)
    (make-attribute attribute (format NIL "~a~@[(~{~a~^, ~})~]" (resolve value/function) (mapcar #'resolve function-args)))))
