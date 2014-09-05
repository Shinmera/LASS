#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *vars* (make-hash-table))

(defun resolve (thing)
  (typecase thing
    (null
     NIL)
    (string
     thing)
    (array
     (gethash (aref thing 0) *vars*))
    (keyword
     (format NIL ":~a" (string-downcase thing)))
    (symbol
     (string-downcase thing))
    (T (princ-to-string thing))))

(defun make-property (property &optional value)
  (list :property property value))

(defun make-block (selector values)
  (cons :block (cons selector values)))

(defgeneric compile-property (key value)
  (:method (key (value list))
    (list (make-property
                (string-downcase key)
                (format NIL "~{~a~^ ~}" (mapcar #'resolve value)))))

  (:method (key value)
    (list (make-property
                (resolve key)
                (resolve value)))))

;; THIS IS SOME PRETTY SHODDY MAGIC CODE HERE
;; BEWARE OF DRAGONS AND ALL THAT
;; YOU HAVE BEEN WARNED.
;;
;; Can't wait for bugs about this to hit me down the line.
(defgeneric compile-constraint (func args)
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
                     do (push (format NIL "~a ~a" func arg) result))
            finally (return (compile-constraint (nreverse result) (cdr args))))))
  (:method ((func null) (args null))
    NIL)
  (:method (func (args null))
    func)
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
                           do (push (format NIL "~a~a" func arg) result))
                  finally (return (compile-constraint :and (cons (cons :OR (nreverse result)) (cddr args))))))
          (if (and (listp (first args)) (eql (first (first args)) :OR))
              (rest (first args))
              args)))))

(defun compile-selector (selector)
  (etypecase selector
    (null NIL)
    ((or symbol string number)
     (list (resolve selector)))
    (list
     (compile-constraint (car selector) (cdr selector)))))

(defgeneric compile-block (header fields)
  (:method (selector fields)
    (let ((selector (compile-selector selector))
          (props ())
          (subblocks ()))
      ;; compute props and subblocks
      (flet ((add-prop (prop)
               (when prop
                 (let ((prop (nreverse prop)))
                   (dolist (prop (compile-property (car prop) (cdr prop)))
                     (push prop props))))))
        (loop with prop = ()
              for field in fields
              do (etypecase field
                   (keyword
                    (add-prop prop)
                    (setf prop (list field)))
                   (list
                    (dolist (subblock (compile-block (list selector (car field)) (cdr field)))
                      (push subblock subblocks)))
                   (T (push field prop)))
              finally (add-prop prop)))
      ;; Returns list of blocks with ours consed to front
      (cons (make-block selector (nreverse props))
            (nreverse subblocks)))))

(defun compile-sheet (&rest blocks)
  (let ((sheet ()))
    (dolist (block blocks)
      (dolist (resulting-block (compile-block (car block) (cdr block)))
        (push resulting-block sheet)))
    (nreverse sheet)))
