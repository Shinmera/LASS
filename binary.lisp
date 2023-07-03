(defpackage #:binary-lass
  (:nicknames #:org.tymoonnext.lass.bianry)
  (:use #:cl)
  (:export
   #:main
   #:cmd-wrapper))
(in-package #:binary-lass)

(defun main (&optional in out (pretty "false"))
  (if in
      (let* ((in (uiop:parse-native-namestring in))
             (out (or (when out (uiop:parse-native-namestring out))
                      (merge-pathnames (make-pathname :type "css") in)))
             (pretty (or (string-equal pretty "true")
                         (string-equal pretty "T"))))
        (lass:generate in :out out :pretty pretty)
        (uiop:native-namestring out))
      (format T "Usage: lass LASS-FILE [ OUTPUT-CSS-FILE [ PRETTY-PRINTING ] ] ~%~%LASS v~a ~a~%"
              (asdf:component-version (asdf:find-system :LASS))
              (asdf:system-homepage (asdf:find-system :LASS)))))

(defun cmd-wrapper (&optional args)
  (let ((args (or args
                  #+SBCL *posix-argv*  
                  #+LISPWORKS system:*line-arguments-list*
                  #+CMU extensions:*command-line-words*
                  #+CCL ccl:*command-line-argument-list*
                  NIL)))
    (handler-case
        (apply #'main (cdr args))
      (error (err)
        (format T "ERROR: ~a" err)))))
