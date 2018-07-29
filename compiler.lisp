#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *vars* (make-hash-table)
  "Special variable containing LASS-environment variables.

See the definition of the LET block.")

(defun read-to-vector (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (assert length)
      (let ((result (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence result stream)
        result))))

(defgeneric resolve (thing)
  (:documentation "Resolves THING to a value that makes sense for LASS.

By default the following types are handled:
NULL:     NIL
STRING:   the THING itself
ARRAY:    the variable stored in *VARS* under THING
KEYWORD:  Colon-prefixed, downcased symbol-name of THING
SYMBOL:   Downcased symbol-name of THING
PATHNAME: If designating an image, base64 encoded inline image data.
T:        PRINC-TO-STRING of THING")
  (:method ((thing null))
    NIL)
  (:method ((thing string))
    thing)
  (:method ((thing array))
    (gethash (aref thing 0) *vars*))
  (:method ((thing symbol))
    (if (keywordp thing)
        (format NIL ":~a" (string-downcase thing))
        (string-downcase thing)))
  (:method ((file pathname))
    (let ((type (mimes:mime-lookup file)))
      (if (and (< 5 (length type))
               (string= type "image" :end1 5))
          (format NIL "url('data:~a;base64,~a')"
                  type (base64:usb8-array-to-base64-string
                        (read-to-vector file)))
          (error "Don't know how to resolve files of type ~a" (or type file)))))
  (:method ((thing T))
    (princ-to-string thing)))

(defun make-property (property &optional value)
  "Creates a property object with PROPERTY as its key and VALUE as its value."
  (list :property property value))

(defun make-block (selector values)
  "Creates a block object with SELECTOR and VALUES."
  (list* :block (list* :selector selector) values))

(defun make-superblock (type selector blocks)
  "Creates a block object that can contain other blocks, such as @media, etc."
  (list* :superblock type (when selector (list* :selector selector)) blocks))

(defgeneric compile-property (key value)
  (:documentation "Compile a property of KEY and VALUE to a list of property objects.
By default, the following cases are handled:

 (T LIST)
A list is created with one property object, wherein the property-value is the
Space-concatenated list of RESOLVEd VALUEs. The KEY is DOWNCASEd.

 (T T)
A list is created with one property object, wherein the property-value is the
RESOLVEd VALUE. The KEY is DOWNCASEd.


Special handling of properties may occur.
See DEFINE-SPECIAL-PROPERTY")
  (:method (key (value list))
    (list (make-property
           (string-downcase key)
           (format NIL "~{~a~^ ~}" (mapcar #'resolve value)))))

  (:method (key value)
    (list (make-property
           (string-downcase key)
           (resolve value)))))

;; THIS IS SOME PRETTY SHODDY MAGIC CODE HERE
;; BEWARE OF DRAGONS AND ALL THAT
;; YOU HAVE BEEN WARNED.
;;
;; Can't wait for bugs about this to hit me down the line.
(defgeneric compile-constraint (func args)
  (:documentation "Compiles a constraint of type FUNC with arguments ARGS to a list of alternative selectors.
By default, the following cases are handled:

 (T T)
Concatenates its ARGS together with spaces.
Preserves OR combinations.

 (NULL NULL)
Returns NIL

 (T NULL)
Returns FUNC

 (:OR T)
Passes all ARGS to COMPILE-SELECTOR individually and then APPENDS
all the results together.

 (:AND T)
Concatenates its ARGS together without spaces.
Preserves OR combinations.


Special handling of constraints may occur.
See DEFINE-SPECIAL-SELECTOR.")
  (:method (func args)
    (let ((cfunc (cond ((listp func)
                        (if (symbolp (first func))
                            (compile-selector func)
                            func))
                       (T (list (resolve func)))))
          (cargs (compile-selector (car args))))
      (loop with result = ()
            for func in cfunc
            do (loop for arg in cargs
                     do (push (list :constraint :child func arg) result))
            finally (return (compile-constraint (nreverse result) (cdr args))))))
  (:method ((func null) (args null))
    NIL)
  (:method (func (args null))
    func)
  (:method ((func (eql :constraint)) args)
    (list (list* func args)))
  (:method ((func (eql :or)) args)
    (apply #'append (mapcar #'compile-selector args)))
  (:method ((func (eql :and)) args)
    (when args
      (if (cdr args)
          (let ((cfunc (compile-selector (first args)))
                (cargs (compile-selector (second args))))
            (loop with result = ()
                  for func in cfunc
                  do (loop for arg in cargs
                           do (push (list :constraint :concat func arg) result))
                  finally (return (compile-constraint :and (cons (cons :OR (nreverse result)) (cddr args))))))
          (if (and (listp (first args)) (eql (first (first args)) :OR))
              (rest (first args))
              args)))))

(defgeneric compile-selector (selector)
  (:documentation "Compiles the SELECTOR form into a list of alternative selectors.
By default, the following cases are handled:

 (NULL)
Returns NIL.

 (LIST)
Calls COMPILE-CONSTRAINT with the SELECTOR's CAR and CDR.

 (T)
Returns a list with the RESOLVEd SELECTOR.")
  (:method ((selector null))
    NIL)
  (:method ((selector list))
    (compile-constraint (car selector) (cdr selector)))
  (:method ((selector T))
    (list (list :constraint :literal (resolve selector)))))

(defgeneric compile-media-constraint (func args)
  (:method (func args)
    (loop for prop in (compile-property func args)
          collect (list* :constraint prop)))
  (:method ((func null) (args null))
    NIL)
  (:method (func (args null))
    func)
  (:method ((func (eql :constraint)) args)
    (list (list* func args)))
  (:method ((func (eql :property)) args)
    (list (list* :constraint func args)))
  (:method ((func (eql :or)) args)
    (loop for arg in args
          nconc (compile-media-query arg)))
  (:method ((func (eql :and)) args)
    (cond ((rest args)
           (let ((cfunc (compile-media-query (first args)))
                 (cargs (compile-media-query (second args))))
             (loop with result = ()
                   for func in cfunc
                   do (loop for arg in cargs
                            do (push (list :constraint :and func arg) result))
                   finally (return (compile-media-constraint :and (list* (list* :or (nreverse result))
                                                                         (cddr args)))))))
          (args
           (compile-media-query (first args)))))
  (:method ((func (eql :url)) args)
    (list (list* :constraint :func "url" (mapcar #'resolve args))))
  (:method ((func (eql :url-prefix)) args)
    (list (list* :constraint :func "url-prefix" (mapcar #'resolve args))))
  (:method ((func (eql :domain)) args)
    (list (list* :constraint :func "domain" (mapcar #'resolve args))))
  (:method ((func (eql :regexp)) args)
    (list (list* :constraint :func "regexp" (mapcar #'resolve args)))))

(defgeneric compile-media-query (query)
  (:method ((query null))
    NIL)
  (:method ((query list))
    (compile-media-constraint (car query) (cdr query)))
  (:method ((query T))
    (list (list :constraint :literal (resolve query)))))

(defgeneric consume-item (item readable-list)
  (:documentation "Consumes items from READABLE-LIST as required by the ITEM.
Returned should be two values, the first being a property to compile (or NIL)
and the second a block to compile (or NIL).
By default, the following cases are handled:

 (LIST)
Simply returns the ITEM as a block.

 (SYMBOL)
If it's a keyword, reads until the next keyword or list and returns that as a
property to compile. With property-functions, some sublists may also be read
in automatically. See DEFINE-PROPERTY-FUNCTION. If it is a regular symbol,
CALL-NEXT-METHOD is invoked.

 (T)
Signals an error.")
  (:method (thing readable-list)
    (error "Don't know what to do with ~s (not part of a property)." thing))
  (:method ((subblock list) readable-list)
    (values
     NIL
     subblock))
  (:method ((property symbol) readable-list)
    (if (keywordp property)
        (values
         (let ((propvals ()))
           (loop for (next fullp) = (multiple-value-list (peek readable-list))
                 while fullp
                 do (etypecase next
                      (keyword (return))
                      (list (return))
                      (T (push (consume readable-list) propvals))))
           (cons property (nreverse propvals)))
         NIL)
        (call-next-method))))

(defgeneric compile-block (header fields)
  (:documentation "Compiles the block with given HEADER and FIELDS list.
By default, the following case is handled:

 (T T)
Blocks are handled in the following way:
The HEADER is used as a selector and compiled through COMPILE-SELECTOR.
Fields are semantically segregated through KEYWORDS and LISTS.

Every time a KEYWORD is encountered, it is taken as the current property
and all following objects until either a LIST or a KEYWORD is encountered
are gathered as the property's values.

Every time a LIST is encountered, it is taken as a SUB-BLOCK and is
passed to COMPILE-BLOCK with the HEADER being the current block's
selector prepended to the selector of the sub-block.


Special handling of blocks may occur. 
See DEFINE-SPECIAL-BLOCK.")
  (:method (selector fields)
    (let ((selector (compile-selector selector))
          (readable (make-readable-list fields))
          (props ())
          (blocks ()))
      ;; compute props and blocks
      (flet ((add-prop (prop)
               (when prop
                 (dolist (prop (compile-property (car prop) (cdr prop)))
                   (push prop props))))
             (add-block (block)
               (when block
                 (dolist (block (compile-block (list selector (car block)) (cdr block)))
                   (push block blocks)))))
        (loop until (empty-p readable)
              for (prop block) = (multiple-value-list (consume-item (consume readable) readable))
              do (add-prop prop)
                 (add-block block)))
      ;; Returns list of blocks with ours consed to front
      (cons (make-block selector (nreverse props))
            (nreverse blocks)))))

(defun compile-sheet (&rest blocks)
  "Compiles a LASS sheet composed of BLOCKS.
Each BLOCK is passed to COMPILE-BLOCK. The results thereof are appended
together into one list of blocks and properties."
  (let ((sheet ()))
    (dolist (block blocks)
      (dolist (resulting-block (compile-block (car block) (cdr block)))
        (push resulting-block sheet)))
    (nreverse sheet)))
