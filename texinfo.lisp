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

(defun texinfo-parse-docstring (docstring bound-args &rest args)
  (apply #'def-properties:parse-docstring docstring bound-args
         :ignore (lambda (word) (member (aref word 0) '(#\-)))
         args))

(defun texinfo-define-function (function-symbol stream)
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
                 (texinfo-parse-docstring
                  (aget function-info :documentation)
                  (def-properties:list-lambda-list-args (aget function-info :arglist)))
                 stream)
                ;; else
                (write-string (aget function-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefun" stream)))))

(defun texinfo-define-variable (variable-symbol stream)
  (let ((variable-info (def-properties:variable-properties variable-symbol)))
    (if (null variable-info)
        (error "Variable properties could not be read: ~s" variable-symbol)
        (progn
          (format stream "@cldefvar {~a, ~a}"
                  (package-name (symbol-package variable-symbol))
                  (symbol-name variable-symbol))
          (terpri stream) (terpri stream)
          (when (aget variable-info :documentation)
            (if (docweaver::read-config :parse-docstrings)
                (texinfo-render-parsed-docstring
                 (texinfo-parse-docstring
                  (aget variable-info :documentation) nil)
                 stream)
                ;; else
                (write-string (aget variable-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefvar" stream)))))

(defun texinfo-define-class (class-symbol stream)
  (let ((class-info (def-properties:class-properties class-symbol)))
    (if (null class-info)
        (error "Class properties could not be read: ~s" class-symbol)
        (progn
          (format stream "@cldefclass {~a, ~a}"
                  (package-name (symbol-package class-symbol))
                  (symbol-name class-symbol))
          (terpri stream) (terpri stream)
          (when (aget class-info :documentation)
            (if (docweaver::read-config :parse-docstrings)
                (texinfo-render-parsed-docstring
                 (texinfo-parse-docstring
                  (aget class-info :documentation) nil)
                 stream)
                ;; else
                (write-string (aget class-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefclass" stream)))))

(def-weaver-command-handler clfunction (function-symbol)
    (:docsystem (eql :texinfo))
  (texinfo-define-function function-symbol stream))

(defun texinfo-format-definitions (symbols stream &key categorized)
  (let ((variables (remove-if-not 'def-properties:symbol-variable-p symbols)))
    (when (and variables categorized)
      (format stream "@heading Variables~%"))
    (dolist (variable variables)
      (texinfo-define-variable variable stream)
      (terpri stream) (terpri stream)))

  (let ((functions (remove-if-not 'def-properties:symbol-function-p symbols)))
    (when (and functions categorized)
      (format stream "@heading Functions~%"))
    (dolist (function functions)
      (texinfo-define-function function stream)
      (terpri stream) (terpri stream)))

  (let ((classes (remove-if-not 'def-properties:symbol-class-p symbols)))
    (when (and classes categorized)
      (format stream "@heading Classes~%"))
    (dolist (class classes)
      (texinfo-define-class class stream)
      (terpri stream) (terpri stream))))

(def-weaver-command-handler clpackage (package-name &key (include-external-definitions t) include-internal-definitions (categorized t))
    (:docsystem (eql :texinfo))
  "Process a clpackage definition.
Defines a package.
If INCLUDE-EXTERNAL-DEFINITIONS is T, then the package external definitions are also defined.
If INCLUDE-INTERNAL-DEFINITIONS is T, then all the package definitions are defined."
  (let ((package (or (find-package (string-upcase package-name))
                     (error "Package not found: ~a" package-name))))
    (format stream "@deftp PACKAGE ~a~%" (package-name package))
    (terpri stream)
    (when (documentation package t)
      (write-string (documentation package t) stream)
      (terpri stream) (terpri stream))
    (format stream "@end deftp")
    (terpri stream) (terpri stream)
    (let (external-symbols internal-symbols)
      (when (or include-internal-definitions
                include-external-definitions)
        (do-external-symbols (symbol package)
          (push symbol external-symbols)))
      (when include-internal-definitions
        (do-symbols (symbol package)
          (when (and (eql (symbol-package symbol) package)
                     (not (member symbol external-symbols)))
            (push symbol internal-symbols))))
      (if (not categorized)
          (progn
            (format stream "@heading External definitions~%~%")
            (texinfo-format-definitions external-symbols stream)
            (when include-internal-definitions
              (format stream "~%@heading Internal definitions~%~%")
              (texinfo-format-definitions internal-symbols stream)))
          ;; else, categorized
          (progn
            (format stream "@heading External definitions~%~%")
            (texinfo-format-definitions external-symbols stream :categorized t)
            (when include-internal-definitions
              (format stream "~%@heading Internal definitions~%~%")
              (texinfo-format-definitions internal-symbols stream :categorized t)))))))

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
