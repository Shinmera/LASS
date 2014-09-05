#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
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
  (:method ((type (eql :block)) block stream)
    (when (and block (cdr block))
      (let ((true-format (format NIL "~a~~{~~a~~^,~@[~* ~]~~}{~:*~@[~*~%~]~~{~~/lass::write-sheet-part/~~^~:*~@[~*~%~]~~}~:*~@[~*~%~]~:*~:*~a~*}"
                                 (indent) *pretty*))
            (*indent-level* (+ *indent-level* 4)))
        (format stream true-format
                (car block) (cdr block)))))
  (:method ((type (eql :property)) attribute stream)
    (when attribute
      (format stream (format NIL "~a~~a~~@[:~@[~* ~]~~a~~];" (indent) *pretty*)
              (first attribute) (second attribute)))))

(defun write-sheet-part (stream block cp ap)
  "Wrapper around WRITE-SHEET-OBJECT so that we can call it from FORMAT.
Calls WRITE-SHEET-OBJECT with (CAR BLOCK) (CDR BLOCK) STREAM."
  (declare (ignore cp ap))
  (write-sheet-object (car block) (cdr block) stream))

(defun write-sheet (sheet &key (stream T) (pretty *pretty*))
  "Writes the compiled SHEET object to STREAM.
If PRETTY is non-NIL, spaces and newlines are inserted as appropriate
in order to create a human-readable stylesheet. Otherwise whitespace is
only used where necessary, producing a minified version.

STREAM can be a STREAM, T for *STANDARD-OUTPUT*, or NIL for a STRING."
  (let ((*pretty* pretty))
    (format stream (format NIL "~~{~~/lass::write-sheet-part/~~^~@[~*~%~%~]~~}" pretty) sheet)))
