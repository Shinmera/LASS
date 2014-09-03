#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *pretty* T)

;; SHEET      ::= (BLOCK*)
;; BLOCK      ::= (SELECTOR ATTRIBUTE*)
;; SELECTOR   ::= (string*)
;; ATTRIBUTE  ::= (string string)

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
    (format stream (format NIL "~~{~~a~~^,~@[~* ~]~~}{~:*~@[~*~%~]~~{~:*~@[~*    ~]~~/lass::write-sheet-attribute/~~^~:*~@[~*~%~]~~}~:*~@[~*~%~]}" *pretty*)
            (car block) (cdr block))))

(defun write-sheet (sheet &key (stream T) (pretty *pretty*))
  (let ((*pretty* pretty))
    (format stream (format NIL "~~{~~/lass::write-sheet-block/~~^~@[~*~%~%~]~~}" pretty) sheet)))
