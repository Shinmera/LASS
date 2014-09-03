#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *pretty* T)
(defvar *sheet*)

;; SHEET      ::= (BLOCK*)
;; BLOCK      ::= (SELECTOR ATTRIBUTE*)
;; SELECTOR   --- string
;; ATTRIBUTE  ::= (NAME VALUE)
;; NAME       --- string
;; VALUE      --- string

(defun selective-downcase (thing)
  (typecase thing
    (string thing)
    (symbol (string-downcase thing))
    (T (princ-to-string thing))))

(defun write-sheet-attribute (stream attribute cp ap)
  (declare (ignore cp ap))
  (when attribute
    (format stream (format NIL "~~a:~@[~* ~]~~a;" *pretty*)
            (first attribute) (second attribute))))

(defun write-sheet-block (stream block cp ap)
  (declare (ignore cp ap))
  (when (and block (cdr block))
    (format stream (format NIL "~~a~@[~* ~]{~:*~@[~*~%~]~~{~:*~@[~*    ~]~~/lass::write-sheet-attribute/~~^~:*~@[~*~%~]~~}~:*~@[~*~%~]}" *pretty*)
            (car block) (cdr block))))

(defun write-sheet (sheet &key (stream T) (pretty *pretty*))
  (let ((*pretty* pretty))
    (format stream (format NIL "~~{~~/lass::write-sheet-block/~~^~@[~*~%~%~]~~}" pretty) sheet)))

(defgeneric compile-attribute (key value)
  (:method (key (value list))
    (list (selective-downcase key)
          (format NIL "~{~a~^ ~}" (mapcar #'selective-downcase value))))

  (:method (key value)
    (list (selective-downcase key)
          (selective-downcase value))))

(defgeneric compile-constraint (type &rest arg)
  (:method ((type (eql 'class))&rest  arg)
    (format NIL ".~a" (selective-downcase (car arg))))

  (:method ((type (eql 'id)) &rest arg)
    (format NIL "#~a" (selective-downcase (car arg))))

  (:method ((type (eql 'tag)) &rest arg)
    (format NIL "~a" (selective-downcase (car arg))))

  (:method ((type (eql 'attr=)) &rest arg)
    (format NIL "[~a=~s]" (selective-downcase (first arg)) (second arg)))

  (:method ((type (eql 'attr~)) &rest arg)
    (format NIL "[~a~~=~s]" (selective-downcase (first arg)) (second arg)))

  (:method ((type (eql 'attr^)) &rest arg)
    (format NIL "[~a^=~s]" (selective-downcase (first arg)) (second arg)))

  (:method ((type (eql 'attr$)) &rest arg)
    (format NIL "[~a$=~s]" (selective-downcase (first arg)) (second arg)))

  (:method ((type (eql 'attr*)) &rest arg)
    (format NIL "[~a*=~s]" (selective-downcase (first arg)) (second arg)))

  (:method ((type (eql 'attr/)) &rest arg)
    (format NIL "[~a|=~s]" (selective-downcase (first arg)) (second arg)))

  (:method (type &rest arg)
    (format NIL "~{~a~^ ~}" (mapcar #'compile-selector (cons type arg)))))

(defgeneric compile-selector (selector)
  (:method ((selector string))
    selector)

  (:method ((selector symbol))
    (selective-downcase selector))

  (:method ((selector list))
    (apply #'compile-constraint selector)))

(defgeneric compile-block (selector body)
  (:method (selector body)
    (let ((values ()))
      (push
       (cons
        (compile-selector selector)
        (let ((body-forms ())
              (attribute NIL))
          (flet ((complete (attribute)
                   (when attribute
                     (let ((args (nreverse attribute)))
                       (setf attribute NIL)
                       (push (compile-attribute (car args) (cdr args))
                             body-forms)))))
            (loop for item in body
                  do (typecase item
                       (keyword
                        (complete attribute)
                        (setf attribute (list item)))
                       ((or string symbol number)
                        (push item attribute))
                       (list
                        (setf values
                              (nconc
                               (compile-block (list selector (car item))
                                              (cdr item))
                               values))))
                  finally (complete attribute)))
          (nreverse body-forms)))
       values)
      values)))

(defgeneric compile-form (form)
  (:method (form)
    (error "???"))
  (:method ((form list))
    (dolist (block (compile-block
                    (car form) (cdr form)))
      (push block *sheet*))))

(defun compile-sheet (&rest forms)
  (let ((*sheet* ()))
    (dolist (form forms)
      (compile-form form))
    (nreverse *sheet*)))
