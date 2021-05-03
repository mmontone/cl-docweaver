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

(def-weaver-command-handler clfunction (function-symbol)
    (:backend (eql :texinfo))
  (let ((function-info (def-properties:function-properties function-symbol)))
    (if (null function-info)
	(error "Function properties could not be read: ~s" function-symbol)
	(progn
	  (format stream "@cldefun {~a, ~a, ~a}"
		  (package-name (symbol-package function-symbol))
		  (symbol-name function-symbol)
		  (aget function-info :args))
	  (terpri stream)
	  (when (aget function-info :documentation)
	    (write-string (aget function-info :documentation) stream))
	  (terpri stream)
	  (write-string "@endcldefun" stream)))))

(defun lget (list key)
  (second (find key list :key 'car)))

(def-weaver-command-handler :clsourceref (symbol &optional type)
    (:backend (eql :texinfo))  
  (let ((symbol-info (ecase (intern (string-upcase type) :keyword)
		       (:function (def-properties:function-properties symbol)))))
    (if (null symbol-info)
	(error "Symbol properties could not be read: ~s" symbol)
	(format stream "@xref{~a, Source}"
		(source-anchor-name (lget (rest (aget symbol-info :source))
					  :file)
				    ;; FIXME: we need the line, not the position here
				    (lget (rest (aget symbol-info :source))
					  :position))))))

(def-weaver-command-handler :clsourcecode (system-name filepath)
    (:backend (eql :texinfo))
  (let ((source-file (asdf:system-relative-pathname system-name filepath)))
    (generate-texinfo-source source-file stream)))

(def-weaver-command-handler :clref (symbol &optional type)
    (:backend (eql :texinfo))
  (princ symbol stream))

#+nil(weave-file
 (asdf:system-relative-pathname :docweaver "test/webinfo.texi")
 (asdf:system-relative-pathname :docweaver "test/webinfo.weaved.texi")
 :backend :texinfo)
