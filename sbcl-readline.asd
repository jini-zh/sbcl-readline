(defpackage :sbcl-readline-system (:use :cl :asdf))
(in-package :sbcl-readline-system)

(defsystem :sbcl-readline
  :name "sbcl-readline"
  :description "GNU Readline support for SBCL"
  :version "0.1"
  :author "Evgeniy Zhemchugov"
  :depends-on (cffi)
  :components ((:file "sbcl-readline")))
