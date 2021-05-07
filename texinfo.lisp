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

(defun texinfo-format (string)
  "Add linebreaks."
  (str:replace-all (coerce (list #\newline) 'string)
		   (coerce (list #\@ #\* #\newline) 'string) string))

(defun source-anchor-name (source-file line-number)
  (format nil "~aL~a" (pathname-name source-file)
          line-number))

(defun qualified-symbol-name (symbol)
  (format nil "~a:~a"
	  (package-name (symbol-package symbol))
	  (symbol-name symbol)))

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

(defun texinfo-format-function (function-symbol stream)
  (let ((function-info (def-properties:function-properties function-symbol)))
    (if (null function-info)
	(error "Function properties could not be read: ~s" function-symbol)
	(progn
	  (format stream "@cldefun {~a, ~a, ~a}"
		  (package-name (symbol-package function-symbol))
		  (symbol-name function-symbol)
		  (aget function-info :args))
	  (terpri stream) (terpri stream)
	  (when (aget function-info :documentation)
	    (if (docweaver::read-config :parse-docstrings)
		(texinfo-render-parsed-docstring
		 (def-properties:parse-docstring
		  (aget function-info :documentation)
		  (def-properties:list-lambda-list-args (aget function-info :arglist)))
		 stream)
		;; else
		(write-string (aget function-info :documentation) stream)))
	  (terpri stream)
	  (write-string "@endcldefun" stream)))))

(def-weaver-command-handler clfunction (function-symbol)
    (:docsystem (eql :texinfo))
  (texinfo-format-function function-symbol stream))

(def-weaver-command-handler clpackage (package-name)
    (:docsystem (eql :texinfo))
  (let ((package (or (find-package (string-upcase package-name))
		     (error "Package not found: ~a" package-name))))
    (format stream "@majorheading ~a~%" (package-name package))
    (terpri stream)
    (when (documentation package t)
      (write-string (documentation package t) stream)
      (terpri stream) (terpri stream))
    (do-external-symbols (symbol package)
      (texinfo-format-function symbol stream)
      (terpri stream) (terpri stream))))

(defun lget (list key)
  (second (find key list :key 'car)))

(def-weaver-command-handler :clsourceref (symbol &optional type)
    (:docsystem (eql :texinfo))  
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
    (:docsystem (eql :texinfo))
  (let ((source-file (asdf:system-relative-pathname system-name filepath)))
    (generate-texinfo-source source-file stream)))



(def-weaver-command-handler :clref (symbol &optional type)
    (:docsystem (eql :texinfo))
  (format stream "@xref{~a, ~a}" (qualified-symbol-name symbol) (symbol-name symbol)))

(defun texinfo-render-parsed-docstring (docstring stream)
  (loop for word in docstring
        do
           (cond
             ((stringp word)
	      (write-string (texinfo-format word) stream))
             ((and (listp word) (eql (car word) :arg))
              (format stream "@var{~a}" (second word)))
             ((and (listp word) (eql (car word) :fn))
	      ;; makeinfo command can be called with --no-validate option for this.
	      ;; in Emacs, customize makeinfo-options variable (add --no-validate option)
	      (format stream "@ref{~a}" (second word))
	      ;;(format stream "@code{~a}" (second word))
	      )
             ((and (listp word) (eql (car word) :var))
	      ;; makeinfo command can be called with --no-validate option for this.
	      ;; in Emacs, customize makeinfo-options variable (add --no-validate option)
              (format stream "@ref{~a}" (second word))
	      ;;(format stream "@var{~a}" (second word))
	      )
             ((and (listp word) (eql (car word) :key))
              (format stream "@var{~a}" (second word))))))

;; (texinfo-render-parsed-docstring (def-properties::parse-docstring "lala :lolo" nil) t)
;; (texinfo-render-parsed-docstring (def-properties::parse-docstring "funcall parse-docstring" nil) t)
;; (texinfo-render-parsed-docstring (def-properties::parse-docstring "asdf" '(asdf)) t)


#+nil(weave-file
 (asdf:system-relative-pathname :docweaver "test/webinfo.texi")
 (asdf:system-relative-pathname :docweaver "test/webinfo.weaved.texi")
 :docsystem :texinfo)
