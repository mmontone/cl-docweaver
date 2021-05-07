;;;; docweaver.asd

(asdf:defsystem #:docweaver
  :description "Documentation weaver for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "docweaver")
	       (:file "texinfo")
	       (:file "markdown"))
  :depends-on (:assoc-utils :str))
