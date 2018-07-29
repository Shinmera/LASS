#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *pretty* T
  "Directs whether to pretty-print using whitespace or not.")
(defvar *indent-level* 0
  "Directs the current amount of spaces used to indent.")

;; SHEET     ::= (BLOCK*)
;; BLOCK     ::= (:BLOCK SELECTOR PROPERTY*)
;; SELECTOR  ::= (string*)
;; PROPERTY  ::= (:PROPERTY string string)

(defun indent ()
  "Returns a string of the appropriate number of spaces depending on *PRETTY* and *INDENT-LEVEL*"
  (make-string (if *pretty* *indent-level* 0) :initial-element #\Space))

(defgeneric write-sheet-object (type object stream)
  (:documentation "Writes the OBJECT of type TYPE to STREAM.

By default the following TYPEs are handled:
 :BLOCK (SELECTOR-LIST OBJECTS*)
Prints the SELECTOR-LIST separated by commas, followed by an opening brace
and the printed list of OBJECTS using WRITE-SHEET-PART. Finally the body is
closed off with a closing brace. Newlines and spaces may be inserted where
necessary if *PRETTY* is non-NIL.

 :PROPERTY (KEY VALUE)
Prints the KEY. If VALUE is non-NIL, a colon is printed followed by the
VALUE. Finally a semicolon is printed. Spaces may be inserted where necessary
if *PRETTY* is non-NIL.")
  (:method ((type (eql :superblock)) block stream)
    (when (and block (cddr block))
      (destructuring-bind (type selector &rest blocks) block
        (format stream "~a@~a" (indent) type)
        (when selector
          (format stream " ")
          (let ((*indent-level* (+ *indent-level* 2 (length type))))
            (write-sheet-object (car selector) (cdr selector) stream)))
        (format stream "{")
        (let ((*indent-level* (+ *indent-level* 4)))
          (dolist (block blocks)
            (when *pretty* (fresh-line stream))
            (write-sheet-object (car block) (cdr block) stream)))
        (format stream "~@[~&~*~]~a}" *pretty* (indent)))))
  
  (:method ((type (eql :block)) block stream)
    (when (and block (cdr block))
      (destructuring-bind (selector &rest body) block
        (format stream "~a" (indent))
        (write-sheet-object (car selector) (cdr selector) stream)
        (format stream "{")
        (let ((*indent-level* (+ *indent-level* 4)))
          (dolist (inner body)
            (when *pretty* (fresh-line stream))
            (write-sheet-object (car inner) (cdr inner) stream)))
        (format stream "~@[~&~*~]~a}" *pretty* (indent)))))

  (:method ((type (eql :property)) attribute stream)
    (when attribute
      (format stream "~a~a~:[~@[:~a~]~;~@[: ~a~]~];" (indent) (first attribute) *pretty* (second attribute))))
  
  (:method ((type (eql :constraint)) constraint stream)
    (ecase (first constraint)
      ((NIL))
      (:child (format stream "~{~/lass::write-sheet-part/~^ ~}" (rest constraint)))
      (:concat (format stream "~{~/lass::write-sheet-part/~}" (rest constraint)))
      (:property (format stream "(~a:~@[~* ~]~a)" (second constraint) *pretty* (third constraint)))
      (:and (format stream "~{~/lass::write-sheet-part/~^ and ~}" (rest constraint)))
      (:func (format stream "~a(~{~a~^,~})" (second constraint) (cddr constraint)))
      (:literal (format stream "~a" (second constraint)))))

  (:method ((type (eql :text)) block stream)
    (when block
      (format stream "~{~a~}~@[~*~%~]" block *pretty*)))

  (:method ((type (eql :selector)) constraints stream)
    (when constraints
      (write-sheet-object (car (first constraints)) (cdr (first constraints)) stream)
      (dolist (constraint (rest constraints))
        (format stream ",")
        (when *pretty* (format stream "~&~a" (indent)))
        (write-sheet-object (car constraint) (cdr constraint) stream)))))

(defun write-sheet-part (stream block cp ap)
  "Wrapper around WRITE-SHEET-OBJECT so that we can call it from FORMAT.
Calls WRITE-SHEET-OBJECT with (CAR BLOCK) (CDR BLOCK) STREAM."
  (declare (ignore cp ap))
  (write-sheet-object (car block) (cdr block) stream))

(defun write-sheet (sheet &key (stream NIL) (pretty *pretty*))
  "Writes the compiled SHEET object to STREAM.
If PRETTY is non-NIL, spaces and newlines are inserted as appropriate
in order to create a human-readable stylesheet. Otherwise whitespace is
only used where necessary, producing a minified version.

STREAM can be a STREAM, T for *STANDARD-OUTPUT*, or NIL for a STRING."
  (let ((*pretty* pretty))
    (format stream (format NIL "~~{~~/lass::write-sheet-part/~~^~@[~*~%~%~]~~}" pretty) sheet)))
