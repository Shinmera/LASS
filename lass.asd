#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem lass
  :name "LASS"
  :version "0.6.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Lisp Augmented Style Sheets. Compiles LASS to CSS."
  :homepage "https://Shinmera.github.io/LASS/"
  :bug-tracker "https://github.com/Shinmera/LASS/issues"
  :source-control (:git "https://github.com/Shinmera/LASS.git")
  :serial T
  :components ((:file "package")
               (:file "readable-list")
               (:file "compiler")
               (:file "property-funcs")
               (:file "writer")
               (:file "lass")
               (:file "special")
               (:file "units")
               (:file "asdf"))
  :depends-on (:trivial-indent
               :trivial-mimes
               :physical-quantities
               :parse-float
               :cl-base64))
