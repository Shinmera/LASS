#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem lass
  :name "LASS"
  :version "0.5.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Lisp Augmented Style Sheets. Compiles LASS to CSS."
  :homepage "https://github.com/Shinmera/LASS"
  :serial T
  :components ((:file "package")
               (:file "readable-list")
               (:file "compiler")
               (:file "property-funcs")
               (:file "writer")
               (:file "lass")
               (:file "special")
               (:file "asdf"))
  :depends-on (:trivial-indent
               :trivial-mimes
               :cl-base64))
