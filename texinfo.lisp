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

(defvar +url-regex+ (ppcre:parse-string "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)")
  "Regular expression for matching URLs. Taken from https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url.")

(defun texinfo-format-urls (string)
  (ppcre:regex-replace-all
   +url-regex+
   string
   "@url{\\&}"))

;;(texinfo-format-urls "foo bar http://google.com lala")

(defun texinfo-format (string &key (parse-urls t)
				(add-line-breaks (getf docweaver::*config* :escape-docstrings)))
  "Add linebreaks and parse urls."
  (let (formatted)
    (setq formatted string)
    (when parse-urls
      (setq formatted (texinfo-format-urls formatted)))
    (when add-line-breaks
      (setq formatted
	    (str:replace-all (coerce (list #\newline) 'string)
			     (coerce (list #\@ #\* #\newline) 'string)
			     formatted)))
    formatted))

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
             (write-string (docweaver::maybe-escape line 'texinfo-escape) output)
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
                  (def-properties:list-lambda-list-args (aget function-info :arglist))
		  :package (symbol-package function-symbol))
                 stream)
                ;; else
                (write-string (aget function-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefun" stream)))))

(defun texinfo-define-macro (macro-symbol stream)
  (let ((macro-info (def-properties:function-properties macro-symbol)))
    (if (null macro-info)
        (error "Macro properties could not be read: ~s" macro-symbol)
        (progn
          (format stream "@cldefmacro {~a, ~a, ~a}"
                  (package-name (symbol-package macro-symbol))
                  (symbol-name macro-symbol)
                  (aget macro-info :args))
          (terpri stream) (terpri stream)
          (when (aget macro-info :documentation)
            (if (docweaver::read-config :parse-docstrings)
                (texinfo-render-parsed-docstring
                 (texinfo-parse-docstring
                  (aget macro-info :documentation)
                  (def-properties:list-lambda-list-args (aget macro-info :arglist))
		  :package (symbol-package macro-symbol))
                 stream)
                ;; else
                (write-string (aget macro-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefmacro" stream)))))

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
                  (aget variable-info :documentation) nil
		  :package (symbol-package variable-symbol))
                 stream)
                ;; else
                (write-string (aget variable-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefvar" stream)))))

(defun texinfo-define-generic-function (function-symbol stream)
  (let ((function-info (def-properties:function-properties function-symbol)))
    (if (null function-info)
        (error "Generic function properties could not be read: ~s" function-symbol)
        (progn
          (format stream "@cldefgeneric {~a, ~a, ~a}"
                  (package-name (symbol-package function-symbol))
                  (symbol-name function-symbol)
                  (aget function-info :args))
          (terpri stream) (terpri stream)
          (when (aget function-info :documentation)
            (if (docweaver::read-config :parse-docstrings)
                (texinfo-render-parsed-docstring
                 (texinfo-parse-docstring
                  (aget function-info :documentation)
                  (def-properties:list-lambda-list-args (aget function-info :arglist))
		  :package (symbol-package function-symbol))
                 stream)
                ;; else
                (write-string (aget function-info :documentation) stream)))
          (terpri stream)
          (write-string "@endcldefgeneric" stream)))))

;; Copied from sb-texinfo:

(defparameter *undocumented-packages* '(sb-pcl sb-int sb-kernel sb-sys sb-c))

(defun ensure-class-precedence-list (class)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (c2mop:class-precedence-list class))

(defun hide-superclass-p (class-name super-name)
  (let ((super-package (symbol-package super-name)))
    (or
     ;; KLUDGE: We assume that we don't want to advertise internal
     ;; classes in CP-lists, unless the symbol we're documenting is
     ;; internal as well.
     (and (member super-package #.'(mapcar #'find-package *undocumented-packages*))
          (not (eq super-package (symbol-package class-name))))
     ;; KLUDGE: We don't generally want to advertise SIMPLE-ERROR or
     ;; SIMPLE-CONDITION in the CPLs of conditions that inherit them
     ;; simply as a matter of convenience. The assumption here is that
     ;; the inheritance is incidental unless the name of the condition
     ;; begins with SIMPLE-.
     (and (member super-name '(simple-error simple-condition))
          (let ((prefix "SIMPLE-"))
            (mismatch prefix (string class-name) :end2 (length prefix)))
          t ; don't return number from MISMATCH
          ))))

(defun texinfo-define-class (class-symbol stream)
  (let ((class-info (def-properties:class-properties class-symbol))
	(class (find-class class-symbol)))
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
	  (terpri stream)
	  (format stream ;"Class precedence list: @code{~(~{@lw{~A}~^, ~}~)}~%~%"
		  "Class precedence list: @code{~(~{~A~^, ~}~)}~%~%"
		  (remove-if (lambda (class)  (hide-superclass-p class-symbol class))
                             (mapcar #'class-name (ensure-class-precedence-list class))))
	  
	  ;; slots
	  (let ((slots (c2mop:class-direct-slots class)))
            (when slots
              (format stream "Slots:~%@itemize~%")
              (dolist (slot slots)
		(format stream
			"@item ~(@code{~A}~#[~:; --- ~]~
                      ~:{~2*~@[~2:*~A~P: ~{@code{@w{~S}}~^, ~}~]~:^; ~}~)~%~%"
			(c2mop:slot-definition-name slot)
			(remove
			 nil
			 (mapcar
			  (lambda (name things)
                            (if things
				(list name (length things) things)))
			  '("initarg" "reader"  "writer")
			  (list
			   (c2mop:slot-definition-initargs slot)
			   (c2mop:slot-definition-readers slot)
			   (c2mop:slot-definition-writers slot)))))
		;; FIXME: Would be neater to handler as children
                (alexandria:when-let (slot-doc (cl:documentation slot t))
                  (write-string slot-doc stream)))
              (format stream "@end itemize~%~%")))
          (write-string "@endcldefclass" stream)))))

(def-weaver-command-handler clvariable (variable-symbol)
    (:docsystem (eql :texinfo))
  (etypecase variable-symbol
    (symbol (texinfo-define-variable variable-symbol stream))
    (list (dolist (var-symbol variable-symbol)
	    (texinfo-define-variable var-symbol stream)
	    (terpri stream)))))

(def-weaver-command-handler clfunction (function-symbol)
    (:docsystem (eql :texinfo))
  (etypecase function-symbol
    (symbol (texinfo-define-function function-symbol stream))
    (list (dolist (fun-symbol function-symbol)
	    (texinfo-define-function fun-symbol stream)
	    (terpri stream)))))

(def-weaver-command-handler clgeneric-function (function-symbol)
    (:docsystem (eql :texinfo))
  (etypecase function-symbol
    (symbol (texinfo-define-generic-function function-symbol stream))
    (list (dolist (fun-symbol function-symbol)
	    (texinfo-define-generic-function fun-symbol stream)
	    (terpri stream)))))

(def-weaver-command-handler clmacro (macro-symbol)
    (:docsystem (eql :texinfo))
  (etypecase macro-symbol
    (symbol (texinfo-define-macro macro-symbol stream))
    (list (dolist (mac-symbol macro-symbol)
	    (texinfo-define-macro mac-symbol stream)
	    (terpri stream)))))

(def-weaver-command-handler clclass (class-name)
  (:docsystem (eql :texinfo))
  (etypecase class-name
    (symbol (texinfo-define-class class-name stream))
    (list (dolist (class-name class-name)
	    (texinfo-define-class class-name stream)
	    (terpri stream)))))

(defun docstring-category (docstring)
  "Extracts category from docstring, when found."
  (let* ((category-regex "Category:\\s*(.*)"))
    (multiple-value-bind (match registers)
	(ppcre:scan-to-strings category-regex
			       docstring)
      (when match
	(aref registers 0)))))
    
(defun symbol-categories (symbol)
  "Collects category for each definition bound to SYMBOL, looking at docstrings."
  (let ((definitions (def-properties:symbol-properties symbol)))
    (mapcar (lambda (def)
	      (cons (or (docstring-category (aget def :documentation))
			"Uncategorized")
		    def))
	    definitions)))

(defun categorize-definitions-by-docstring (symbols)
  (let ((groups (groupby:groupby 'car (apply #'append (mapcar 'symbol-categories symbols))
				 :test 'equalp)))
    (mapcar (lambda (category)
	      (cons (car category) (mapcar 'cdr (cadr category))))
	    groups)))

(defun texinfo-format-definitions-uncategorized (symbols stream)
  (let ((variables (remove-if-not 'def-properties:symbol-variable-p symbols)))
    (dolist (variable variables)
      (texinfo-define-variable variable stream)
      (terpri stream) (terpri stream)))
  (let ((macros (remove-if-not 'def-properties::symbol-macro-p symbols)))
    (dolist (macro macros)
      (texinfo-define-macro macro stream)
      (terpri stream) (terpri stream)))
  (let ((functions (remove-if-not 'def-properties:symbol-generic-function-p symbols)))
    (dolist (function functions)
      (texinfo-define-generic-function function stream)
      (terpri stream) (terpri stream)))
  (let ((functions (remove-if-not 'def-properties:symbol-function-p symbols)))
    (dolist (function functions)
      (texinfo-define-function function stream)
      (terpri stream) (terpri stream)))
  (let ((classes (remove-if-not 'def-properties:symbol-class-p symbols)))
    (dolist (class classes)
      (texinfo-define-class class stream)
      (terpri stream) (terpri stream))))

(defun texinfo-format-definitions-by-kind (symbols stream)
  (let ((variables (remove-if-not 'def-properties:symbol-variable-p symbols)))
    (when variables
      (format stream "@subheading Variables~%")
      (dolist (variable variables)
	(texinfo-define-variable variable stream)
	(terpri stream) (terpri stream))))

  (let ((macros (remove-if-not 'def-properties::symbol-macro-p symbols)))
    (when macros
      (format stream "@subheading Macros~%")
      (dolist (macro macros)
	(texinfo-define-macro macro stream)
	(terpri stream) (terpri stream))))

  (let ((functions (remove-if-not 'def-properties:symbol-generic-function-p symbols)))
    (when functions
      (format stream "@subheading Generic functions~%")
      (dolist (function functions)
	(texinfo-define-generic-function function stream)
	(terpri stream) (terpri stream))))

  (let ((functions (remove-if-not 'def-properties:symbol-function-p symbols)))
    (when functions
      (format stream "@subheading Functions~%")
      (dolist (function functions)
	(texinfo-define-function function stream)
	(terpri stream) (terpri stream))))

  (let ((classes (remove-if-not 'def-properties:symbol-class-p symbols)))
    (when classes
      (format stream "@subheading Classes~%")
      (dolist (class classes)
	(texinfo-define-class class stream)
	(terpri stream) (terpri stream))))

  )

(defun texinfo-format-definition (definition stream)
  (ecase (aget definition :type)
    (:function (texinfo-define-function (aget definition :name) stream))
    (:generic-function (texinfo-define-generic-function (aget definition :name) stream))
    (:macro (texinfo-define-macro (aget definition :name) stream))
    (:variable (texinfo-define-variable (aget definition :name) stream))
    (:class (texinfo-define-class (aget definition :name) stream))))

(defun texinfo-format-definitions-by-docstring-category (symbols stream)
  (let ((categories (categorize-definitions-by-docstring symbols)))
    (dolist (category categories)
      (let ((category-name (car category))
	    (definitions (cdr category)))
	(format stream "@subheading ~a~%~%" category-name)
	(dolist (def definitions)
	  (texinfo-format-definition def stream)
	  (terpri stream) (terpri stream))))))

(defun texinfo-format-definitions (symbols stream &key (categorized t))
  (check-type categorized (or boolean (member :by-kind :by-docstring-category)))
  (case categorized
    ((:by-kind t) (texinfo-format-definitions-by-kind symbols stream))
    (:by-docstring-category (texinfo-format-definitions-by-docstring-category symbols stream))
    (t (texinfo-format-definitions-uncategorized symbols stream))))    

(def-weaver-command-handler clpackage
    (package-name
     &key
     (include-external-definitions t)
     include-internal-definitions
     (include-undocumented-definitions t)
     (categorized t))
    (:docsystem (eql :texinfo))
  "Process a clpackage definition.
Defines a package.
If INCLUDE-EXTERNAL-DEFINITIONS is T, then the package external definitions are also defined.
If INCLUDE-INTERNAL-DEFINITIONS is T, then all the package definitions are defined.
INCLUDE-UNDOCUMENTED-DEFINITIONS controls if definitions are included depending on wether they have a docstring or not.
CATEGORIZED controls how to categorize the expanded package definitions:
- :by-kind or T, definitions are separated in sections (variables, functions, etc).
- :by-docstring-category, definitions are grouped by the category parsed from docstrings. 
- Otherwise, they are expanded in sequence with no separation."
  (let ((package (or (find-package (string-upcase package-name))
                     (error "Package not found: ~a" package-name))))
    (format stream "@deftp PACKAGE ~a~%" (package-name package))
    (terpri stream)
    (when (documentation package t)
      (write-string (texinfo-format (docweaver::maybe-escape (documentation package t) 'texinfo-escape)) stream)
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
            (texinfo-format-definitions external-symbols stream :categorized categorized)
            (when include-internal-definitions
              (format stream "~%@heading Internal definitions~%~%")
              (texinfo-format-definitions internal-symbols stream :categorized categorized)))))))

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

(def-weaver-command-handler :clref (symbol type)
    (:docsystem (eql :texinfo))
  (format stream "@clref{~a, ~a, ~a}"
	  (package-name (symbol-package symbol))
	  (symbol-name symbol)
	  (string-downcase (princ-to-string type))))

(defun texinfo-render-parsed-docstring (docstring stream)
  (loop for word in docstring
        do
           (cond
             ((stringp word)
              (write-string (texinfo-format (docweaver::maybe-escape word 'texinfo-escape)) stream))
             ((and (listp word) (eql (car word) :arg))
              (format stream "@var{~a}" (second word)))
	     ((and (listp word) (eql (car word) :special-operator))
              (format stream "@code{~a}" (second word)))
	     ((and (listp word) (eql (car word) :macro))
              ;; makeinfo command can be called with --no-validate option for this.
              ;; in Emacs, customize makeinfo-options variable (add --no-validate option)
              (format stream "@clref{~a, ~a, macro}"
		      (package-name (symbol-package (third word)))
		      (second word))
              ;;(format stream "@code{~a}" (second word))
              )
             ((and (listp word) (eql (car word) :fn))
              ;; makeinfo command can be called with --no-validate option for this.
              ;; in Emacs, customize makeinfo-options variable (add --no-validate option)
              (format stream "@clref{~a, ~a, function}"
		      (package-name (symbol-package (third word)))
		      (second word))
              ;;(format stream "@code{~a}" (second word))
              )
             ((and (listp word) (eql (car word) :var))
              ;; makeinfo command can be called with --no-validate option for this.
              ;; in Emacs, customize makeinfo-options variable (add --no-validate option)
	      (format stream "@clref{~a, ~a, variable}"
		      (package-name (symbol-package (third word)))
		      (second word))
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
