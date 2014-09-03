#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.lass)

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
    
    ))

(defgeneric compile-block (header fields)
  (:method (selector fields)
    (let ((selector (compile-selector selector))
          (attrs ())
          (subblocks ()))
      ;; compute attrs and subblocks
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
                    (dolist (subblock (compile-block (list selector (car field)) (cdr field)))
                      (push subblock subblocks)))
                   (T (push field attr)))
              finally (add-attr attr)))
      
      (cons (cons :block (cons selector (nreverse attrs)))
            (nreverse subblocks)))))

(defun compile-sheet (&rest blocks)
  (let ((sheet ()))
    (dolist (block blocks)
      (dolist (resulting-block (compile-block (car block) (cdr block)))
        (push resulting-block sheet)))
    (nreverse sheet)))
