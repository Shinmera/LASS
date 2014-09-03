#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

(defvar *sheet*)

(defgeneric compile-attribute (key value)
  (:method (key (value list))
    (list (list :attribute
                (selective-downcase key)
                (format NIL "~{~a~^ ~}" (mapcar #'selective-downcase value)))))

  (:method (key value)
    (list (list :attribute
                (selective-downcase key)
                (selective-downcase value)))))

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
                       (T (list (selective-downcase func)))))
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
                  finally (return (compile-constraint :and (append (nreverse result) (cddr args))))))
          args))))

(defun compile-selector (selector)
  (etypecase selector
    (null NIL)
    ((or symbol string number)
     (list (selective-downcase selector)))
    (list
     (compile-constraint (car selector) (cdr selector)))))

(defun process-attrs (selector fields)
  (let ((attrs ()))
    (flet ((add-attr (attr)
             (when attr
               (let ((attr (nreverse attr)))
                 (dolist (attr (compile-attribute (car attr) (cdr attr)))
                   (push attr attrs))))))
      (loop with attr = ()
            for field in fields
            do (etypecase field
                 (keyword
                  (add-attr attr)
                  (setf attr (list field)))
                 (list
                  (compile-block (list selector (car field)) (cdr field)))
                 (T (push field attr)))
            finally (add-attr attr)))
    (nreverse attrs)))

;; This function contains a lot of shuffling about of the *SHEET*
;; this is annoying because we need to constantly push things, but
;; also always need to preserve order of the elements, so there's
;; a lot of reversing back and forth.
(defgeneric compile-block (header fields)
  (:method (selector fields)
    (let ((selector (compile-selector selector))
          (attrs ())
          (subblocks ()))
      (let* ((*sheet* ()))
        (setf attrs (process-attrs selector fields))
        (setf subblocks (nreverse *sheet*)))
      (push
       (cons
        :block
        (cons
         selector
         attrs))
       *sheet*)
      (dolist (block subblocks)
        (push block *sheet*))
      *sheet*)))

(defun compile-sheet (&rest blocks)
  (let ((*sheet* ()))
    (dolist (block blocks)
      (compile-block (car block) (cdr block)))
    (nreverse *sheet*)))
