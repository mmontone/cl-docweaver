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

(defmethod process-weaver-syntax ((backend (eql :markdown))
                                  (syntax (eql :@clfunction)) line stream)
  (let ((regex (aget *weaver-syntax* :@clfunction)))
    (ppcre:do-register-groups (package-name symbol-name)
        (regex line)
      (let* ((function-symbol (intern (string-upcase symbol-name)
                                      (or (find-package (string-upcase package-name))
                                          (error "Package not found: ~a" package-name))))
             (function-info (def-properties:function-properties function-symbol)))
        (if (null function-info)
            (error "Function properties could not be read: ~s" function-symbol)
            (progn
              (format stream "### function ~a:~a ~a"
                      package-name symbol-name (aget function-info :args))
              (terpri stream)
              (when (aget function-info :documentation)
                (write-string (aget function-info :documentation) stream))
              (terpri stream)))))))

(defmethod process-weaver-syntax ((backend (eql :markdown))
                                  (syntax (eql :@clvariable)) line stream)
  (let ((regex (aget *weaver-syntax* :@clvariable)))
    (ppcre:do-register-groups (package-name symbol-name)
        (regex line)
      (let* ((function-symbol (intern (string-upcase symbol-name)
                                      (or (find-package (string-upcase package-name))
                                          (error "Package not found: ~a" package-name))))
             (function-info (def-properties:function-properties function-symbol)))
        (if (null function-info)
            (error "Function properties could not be read: ~s" function-symbol)
            (progn
              (format stream "@cldefun {~a, ~a, ~a}"
                      package-name symbol-name (aget function-info :args))
              (terpri stream)
              (when (aget function-info :documentation)
                (write-string (aget function-info :documentation) stream))
              (terpri stream)
              (write-string "@endcldefun" stream)))))))

(defun lget (list key)
  (second (find key list :key 'car)))

(defmethod process-weaver-syntax ((backend (eql :markdown))
				  (syntax (eql :@clsourceref)) line stream)
  "TODO"
  )

(defmethod process-weaver-syntax ((backend (eql :markdown))
				  (syntax (eql :@clsourcecode)) line stream)
  (let ((regex (aget *weaver-syntax* :@clsourcecode)))
    (ppcre:do-register-groups (system-name filepath)
        (regex line)
      (let ((source-file (asdf:system-relative-pathname system-name filepath)))
	(generate-markdown-source source-file stream)))))

#+nil(weave-file
 (asdf:system-relative-pathname :docweaver "test/webinfo.md")
 (asdf:system-relative-pathname :docweaver "test/webinfo.weaved.md")
 :markdown)
