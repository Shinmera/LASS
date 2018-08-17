#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defsystem binary-lass
  :name "LASS Binary"
  :version "0.1.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "System to create a binary executable for LASS."
  :homepage "https://Shinmera.github.io/LASS/"
  :bug-tracker "https://github.com/Shinmera/LASS/issues"
  :source-control (:git "https://github.com/Shinmera/LASS.git")
  :serial T
  :components ((:file "binary"))
  :depends-on (:lass))
