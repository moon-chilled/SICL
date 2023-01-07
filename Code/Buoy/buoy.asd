(cl:in-package #:asdf-user)

(defsystem :buoy
  :serial t
  :depends-on ("float-features")
  :components ((:file "package")
	       (:file "fma")
               (:file "basics")
	       (:file "scale-float-defun")
	       (:file "sin-cos-table")
	       (:file "float-bits")
	       (:file "sin-cos-single-defun")))
