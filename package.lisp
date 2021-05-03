;;;; package.lisp

(defpackage #:docweaver
  (:use #:cl #:assoc-utils)
  (:export :process-weaver-syntax
   :*weaver-syntax*
   :weave-file))
