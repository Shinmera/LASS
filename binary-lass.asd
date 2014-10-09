#|
 This file is a part of LASS
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.lass.binary.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.lass.binary.asdf)

(defsystem binary-lass
  :name "LASS Binary"
  :version "0.1.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "System to create a binary executable for LASS."
  :homepage "https://github.com/Shinmera/LASS"
  :serial T
  :components ((:file "binary"))
  :depends-on (:lass))
