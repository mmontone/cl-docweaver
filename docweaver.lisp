;;;; docweaver.lisp

(in-package #:docweaver)

;; This embeds a CL function, reading its structure and documentation from Lisp process:
;; @clfunction{alexandria:flatten}
;; Same for macros and other CL stuff:
;; @clmacro{cl:with-open-file}

(defparameter *weaver-syntax*
  '((:@clfunction . "@clfunction{(.*),(.*)}")
    (:@clvariable . "@clvariable{(.*),(.*)}")
    (:@clmacro . "@clmacro{(.*),(.*)}")
    (:@clclass . "@clclass{(.*),(.*)}")
    (:@clpackage . "@clpackage{(.*)}")
    (:@clsystem . "@clsystem{(.*)}")
    (:@clsourcecode . "@clsourcecode{(.*),(.*)}")
    (:@clsourceref . "@clsourceref{(.*),(.*),(.*)}")
    ))

(defgeneric backend-weave-file (backend file stream))

(defmethod backend-weave-file (backend file stream)
  "Expands @clfunction, @clmacro, etc. definitions to a Texinfo definition with body extracted from Common Lisp code."
  (with-open-file (f file :direction :input
                          :external-format :utf-8)
    (loop for line := (read-line f nil nil)
          while line
          do
             (when (not
                    (block process
                      (dolist (syntax *weaver-syntax*)
                        (when (ppcre:scan (cdr syntax) line)
                          (process-weaver-syntax backend (car syntax) line stream)
                          (terpri stream)
                          (return-from process t)))))
               (write-string line stream)
               (terpri stream)))))

(defgeneric process-weaver-syntax (backend syntax line stream))

;; @clpackage-functions: Produce a Texinfo section with a package external function definitions.
;; @clpackage-variables: Same with variables.
;; @clpackage-classes: Same with classes.

;; TODO: make @cldefun reference source code when enabled (use swank location).
;; Source code enabled is indicated with a Texinfo variable.
;; Source code is serialized to a Texinfo node with an anchor for each line @anchor{<filename>L<linename>}


;; an idea could be to use the following via an @clpackage-reference{package-name} macro that expands to a full Texinfo chapter with definitions
(defun generate-texinfo-reference-for-package (package stream)
  "Generates a Texinfo reference with PACKAGE external symbols documentation")


(defun weave-file (file output-file backend)
  (with-open-file (output output-file :direction :output
                                      :external-format :utf-8
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
    (backend-weave-file backend file output)))

#+nil(weave-file
 (asdf:system-relative-pathname :webinfo "test/webinfo.texi")
 (asdf:system-relative-pathname :webinfo "test/webinfo.processed.texi")
 :texinfo)
