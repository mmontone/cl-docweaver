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

(defun indent-text (text indentation)
  (with-output-to-string (output)
    (with-input-from-string (input text)
      (loop for line := (read-line input nil nil)
	    while line
	    do (dotimes (i indentation)
		 (write-char #\space output))
	       (write-line line output)
	       ))))

(defun markdown-format-function (function-symbol stream)
  (let ((function-info (def-properties:function-properties function-symbol)))
    (if (null function-info)
	(error "Function properties could not be read: ~s" function-symbol)
	(progn
	  (format stream "- [function] **~a:~a** *~a*~%"
		  (package-name (symbol-package function-symbol))
		  (symbol-name function-symbol)
		  (aget function-info :args))
	  (terpri stream)
	  (when (aget function-info :documentation)
	    (write-string (indent-text (aget function-info :documentation) 4) stream))
	  (terpri stream)))))

(defun markdown-format-generic-function (function-symbol stream)
  (let ((function-info (def-properties:function-properties function-symbol)))
    (if (null function-info)
	(error "Function properties could not be read: ~s" function-symbol)
	(progn
	  (format stream "- [generic function] **~a:~a** *~a*~%"
		  (package-name (symbol-package function-symbol))
		  (symbol-name function-symbol)
		  (aget function-info :args))
	  (terpri stream)
	  (when (aget function-info :documentation)
	    (write-string (indent-text (aget function-info :documentation) 4) stream))
	  (terpri stream)))))

(def-weaver-command-handler clfunction (function-symbol)
    (:docsystem (eql :markdown))
  (markdown-format-function function-symbol stream))

(def-weaver-command-handler clgeneric-function (function-symbol)
    (:docsystem (eql :markdown))
  (markdown-format-generic-function function-symbol stream))

(def-weaver-command-handler clvariable (variable-symbol)
    (:docsystem (eql :markdown))
  (let ((variable-info (def-properties:variable-properties variable-symbol)))
        (if (null variable-info)
            (error "Variable properties could not be read: ~s" variable-symbol)
            (progn
              (format stream "- [variable] **~a:~a**~%"
                      (package-name (symbol-package variable-symbol))
		      (symbol-name variable-symbol))
              (terpri stream)
              (when (aget variable-info :documentation)
                (write-string (indent-text (aget variable-info :documentation) 4) stream))
              (terpri stream)
              ))))

(defun lget (list key)
  (second (find key list :key 'car)))

(def-weaver-command-handler clsourceref (symbol)
    (:docsystem (eql :markdown))
  "TODO"
  )

(def-weaver-command-handler clsourcecode (system-name filepath)
  (:docsystem (eql :markdown))
  (let ((source-file (asdf:system-relative-pathname system-name filepath)))
    (generate-markdown-source source-file stream)))

(def-weaver-command-handler clref (symbol &optional type)
    (:docsystem (eql :markdown))
  (princ symbol stream))

(def-weaver-command-handler clpackage (package-name &rest options)
    (:docsystem (eql :markdown))
  (let ((package (or (find-package (string-upcase package-name))
		     (error "Package not found: ~a" package-name))))
    (format stream "## ~a~%" (package-name package))
    (terpri stream)
    (when (documentation package t)
      (write-string (documentation package t) stream)
      (terpri stream) (terpri stream))
    (do-external-symbols (symbol package)
      (markdown-format-function symbol stream)
      (terpri stream) (terpri stream))))

#+nil(weave-file
 (asdf:system-relative-pathname :docweaver "test/webinfo.md")
 (asdf:system-relative-pathname :docweaver "test/webinfo.weaved.md")
 :docsystem :markdown)
