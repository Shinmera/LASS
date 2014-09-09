#|
 This file is a part of LASS
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.lass.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.lass.asdf)

(defsystem lass
  :name "LASS"
  :version "0.2.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Lisp Augmented Style Sheets. Compiles LASS to CSS."
  :homepage "https://github.com/Shinmera/LASS"
  :serial T
  :components ((:file "package")
               (:file "compiler")
               (:file "writer")
               (:file "lass")
               (:file "special"))
  :depends-on (:trivial-indent
               :trivial-mimes
               :cl-base64))
