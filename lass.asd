(defsystem lass
  :name "LASS"
  :version "0.6.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Lisp Augmented Style Sheets. Compiles LASS to CSS."
  :homepage "https://shinmera.com/docs/LASS/"
  :bug-tracker "https://shinmera.com/project/LASS/issues"
  :source-control (:git "https://shinmera.com/project/LASS.git")
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
