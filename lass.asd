(defsystem lass
  :name "LASS"
  :version "0.6.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
               (:file "asdf"))
  :depends-on (:trivial-indent
               :trivial-mimes
               :cl-base64))
