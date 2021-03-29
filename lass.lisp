#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defmacro define-special-block (name args &body body)
  "Define handling of a special block type.
In order for the block to be recognised, it has to begin with the NAME as a keyword.
The ARGS is a lambda-list that parses the block contents.

Expected as a return value is a /list/ of either attributes or blocks.

See COMPILE-BLOCK"
  (let ((argsym (gensym "ARGS")))
    `(defmethod compile-block ((,(gensym "TYPE") (eql ,(intern (string name) "KEYWORD"))) ,argsym)
       (destructuring-bind ,args ,argsym
         ,@body))))

(defmacro define-special-property (name args &body body)
  "Define handling of a special property.
The ARGS is a lambda-list that parses the arguments passed to the property.

Expected as a return value is a /list/ of either attributes or blocks.

See COMPILE-PROPERTY"
  (let ((argsym (gensym "ARGS")))
    `(defmethod compile-property ((,(gensym "ATTR") (eql ,(intern (string name) "KEYWORD"))) ,argsym)
       (destructuring-bind ,args ,argsym
         ,@body))))

(defmacro define-special-selector (name args &body body)
  "Define handling of a special selector type.
In order for the selector to be recognised, it has to begin with the NAME as a keyword.
The ARGS is a lambda-list that parses the selector contents.

Expected as a return value is a /list/ of alternate versions of selectors.

See COMPILE-CONSTRAINT."
  (let ((argsym (gensym "ARGS")))
    `(defmethod compile-constraint ((,(gensym "FUNC") (eql ,(intern (string name) "KEYWORD"))) ,argsym)
       (destructuring-bind ,args ,argsym
         ,@body))))

(defun generate (in &key (out (merge-pathnames (make-pathname :type "css") in)) (pretty NIL) (if-exists :supersede))
  "Generate a CSS file from a LASS file.

IN        --- The LASS input file. Has to be READable.
OUT       --- The target file, by default a file of same location and name, but with CSS type.
PRETTY    --- Whether to minify or not. See WRITE-SHEET.
IF-EXISTS --- See WITH-OPEN-FILE

Returns OUT"
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

(defun compile-and-write (&rest forms)
  "Shortcut for (WRITE-SHEET (COMPILE-SHEET FORMS*))"
  (write-sheet
   (apply #'compile-sheet
          forms)))
