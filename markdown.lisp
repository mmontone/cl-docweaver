(defpackage :docweaver/markdown
  (:use :cl :docweaver :assoc-utils))

(in-package :docweaver/markdown)

(defun markdown-escape (string)
  (let ((chars
          (loop
            for char across string
            if (member char '(#\{ #\} #\@))
              collect #\@ and collect char
            else
              collect char)))
    (coerce chars 'string)))

(defun generate-markdown-source (source-file output)
  (with-open-file (f source-file :direction :input
                                 :external-format :utf-8)
    (write-line "```lisp" output)
    (loop for line := (read-line f nil nil)
          for line-number := 1 then (1+ line-number)
          while line
          do
	     (write-string (markdown-escape line) output)
	     (terpri output))
    (write-line "```" output)))

(def-weaver-command-handler :clfunction (function-symbol)
    (:backend (eql :markdown))
  (let ((function-info (def-properties:function-properties function-symbol)))
    (if (null function-info)
	(error "Function properties could not be read: ~s" function-symbol)
	(progn
	  (format stream "### function ~a:~a ~a"
		  (package-name (symbol-package function-symbol))
		  (symbol-name function-symbol)
		  (aget function-info :args))
	  (terpri stream)
	  (when (aget function-info :documentation)
	    (write-string (aget function-info :documentation) stream))
	  (terpri stream)))))

(def-weaver-command-handler :clvariable (variable-symbol)
    (:backend (eql :markdown))
  (let ((variable-info (def-properties:variable-properties variable-symbol)))
        (if (null variable-info)
            (error "Variable properties could not be read: ~s" variable-symbol)
            (progn
              (format stream "@cldefvar {~a, ~a, ~a}"
                      (package-name (symbol-package variable-symbol))
		      (symbol-name variable-symbol)
		      (aget variable-info :args))
              (terpri stream)
              (when (aget variable-info :documentation)
                (write-string (aget variable-info :documentation) stream))
              (terpri stream)
              (write-string "@endcldefvar" stream)))))

(defun lget (list key)
  (second (find key list :key 'car)))

(def-weaver-command-handler :clsourceref (symbol)
    (:backend (eql :markdown))
  "TODO"
  )

(def-weaver-command-handler :clsourcecode (system-name filepath)
  (:backend (eql :markdown))
  (let ((source-file (asdf:system-relative-pathname system-name filepath)))
    (generate-markdown-source source-file stream)))

#+nil(weave-file
 (asdf:system-relative-pathname :docweaver "test/webinfo.md")
 (asdf:system-relative-pathname :docweaver "test/webinfo.weaved.md")
 :backend :markdown)
