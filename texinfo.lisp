(defpackage :docweaver/texinfo
  (:use :cl :docweaver :assoc-utils))

(in-package :docweaver/texinfo)

(defun texinfo-escape (string)
  (let ((chars
          (loop
            for char across string
            if (member char '(#\{ #\} #\@))
              collect #\@ and collect char
            else
              collect char)))
    (coerce chars 'string)))

(defun source-anchor-name (source-file line-number)
  (format nil "~aL~a" (pathname-name source-file)
          line-number))

(defun generate-texinfo-source (source-file output)
  "Source code is serialized to a Texinfo node with an anchor for each line @anchor{<filename>L<linename>}"
  (with-open-file (f source-file :direction :input
                                 :external-format :utf-8)
    (loop for line := (read-line f nil nil)
          for line-number := 1 then (1+ line-number)
          while line
          do
             (format output "@anchor{~a}" (source-anchor-name source-file line-number))
             (write-string (texinfo-escape line) output)
	     (write-string "@*" output)
	     (terpri output))))

(defmethod process-weaver-syntax ((backend (eql :texinfo))
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
              (format stream "@cldefun {~a, ~a, ~a}"
                      package-name symbol-name (aget function-info :args))
              (terpri stream)
              (when (aget function-info :documentation)
                (write-string (aget function-info :documentation) stream))
              (terpri stream)
              (write-string "@endcldefun" stream)))))))

(defmethod process-weaver-syntax ((backend (eql :texinfo))
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

(defmethod process-weaver-syntax ((backend (eql :texinfo))
				  (syntax (eql :@clsourceref)) line stream)
  (let ((regex (aget *weaver-syntax* :@clsourceref)))
    (ppcre:do-register-groups (symbol-type package-name symbol-name)
        (regex line)
      (let* ((symbol (intern (string-upcase symbol-name)
                             (or (find-package (string-upcase package-name))
                                 (error "Package not found: ~a" package-name))))
             (symbol-info (ecase (intern (string-upcase symbol-type) :keyword)
                            (:function (def-properties:function-properties symbol)))))
        (if (null symbol-info)
            (error "Symbol properties could not be read: ~s" symbol)
            (format stream "@xref{~a, Source}"
                    (source-anchor-name (lget (rest (aget symbol-info :source))
                                              :file)
                                        ;; FIXME: we need the line, not the position here
                                        (lget (rest (aget symbol-info :source))
                                              :position))))))))

(defmethod process-weaver-syntax ((backend (eql :texinfo))
				  (syntax (eql :@clsourcecode)) line stream)
  (let ((regex (aget *weaver-syntax* :@clsourcecode)))
    (ppcre:do-register-groups (system-name filepath)
        (regex line)
      (let ((source-file (asdf:system-relative-pathname system-name filepath)))
	(generate-texinfo-source source-file stream)))))

#+nil(weave-file
 (asdf:system-relative-pathname :docweaver "test/webinfo.texi")
 (asdf:system-relative-pathname :docweaver "test/webinfo.processed.texi")
 :texinfo)
