(defsystem binary-lass
  :name "LASS Binary"
  :version "0.1.1"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "System to create a binary executable for LASS."
  :homepage "https://shinmera.com/docs/LASS/"
  :bug-tracker "https://shinmera.com/project/LASS/issues"
  :source-control (:git "https://shinmera.com/project/LASS.git")
  :serial T
  :components ((:file "binary"))
  :depends-on (:lass))
