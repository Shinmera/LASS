#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *pretty* T)
(defvar *indent-level* 0)

;; SHEET      ::= (BLOCK*)
;; BLOCK      ::= (:BLOCK SELECTOR ATTRIBUTE*)
;; SELECTOR   ::= (string*)
;; ATTRIBUTE  ::= (:ATTRIBUTE string string)

(defun selective-downcase (thing)
  (typecase thing
    (string thing)
    (symbol (string-downcase thing))
    (T (princ-to-string thing))))

(defun indent ()
  (make-string (if *pretty* *indent-level* 0) :initial-element #\Space))

(defgeneric write-sheet-object (type object stream)
  (:method ((type (eql :block)) block stream)
    (when (and block (cdr block))
      (let ((true-format (format NIL "~a~~{~~a~~^,~@[~* ~]~~}{~:*~@[~*~%~]~~{~~/lass::write-sheet-part/~~^~:*~@[~*~%~]~~}~:*~@[~*~%~]~:*~:*~a~*}"
                                 (indent) *pretty*))
            (*indent-level* (+ *indent-level* 4)))
        (format stream true-format
                (car block) (cdr block)))))
  (:method ((type (eql :attribute)) attribute stream)
    (when attribute
      (format stream (format NIL "~a~~a~~@[:~@[~* ~]~~a~~];" (indent) *pretty*)
              (first attribute) (second attribute)))))

(defun write-sheet-part (stream block cp ap)
  (declare (ignore cp ap))
  (write-sheet-object (car block) (cdr block) stream))

(defun write-sheet (sheet &key (stream T) (pretty *pretty*))
  (let ((*pretty* pretty))
    (format stream (format NIL "~~{~~/lass::write-sheet-part/~~^~@[~*~%~%~]~~}" pretty) sheet)))
