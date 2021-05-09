;;;; docweaver.lisp

(require :def-properties (asdf:system-relative-pathname :docweaver "def-properties.lisp"))

(in-package #:docweaver)

;; This embeds a CL function, reading its structure and documentation from Lisp process:
;; @clfunction{alexandria:flatten}
;; Same for macros and other CL stuff:
;; @clmacro{cl:with-open-file}

(defun package-symbols (package)
  "Return all symbols that belong to PACKAGE (both internal and external)."
  (let (symbols)
    (do-symbols (symbol package)
      (when (eql (symbol-package symbol) package)
	(push symbol symbols)))
    symbols))

(defun package-variables (package)
  (remove-if-not 'boundp (package-symbols package)))

(defun package-functions (package)
  (remove-if-not 'fboundp (package-symbols package)))

(defgeneric docsystem-weave-file (docsystem file stream))

(defmethod docsystem-weave-file (docsystem file stream)
  "Expands @clfunction, @clmacro, etc. definitions to a Texinfo definition with body extracted from Common Lisp code."
  (with-open-file (f file :direction :input
                          :external-format :utf-8)
    (loop for line := (read-line f nil nil)
          while line
          do
             (process-weaver-commands docsystem line stream)
	     (terpri stream))))

(defun peek-n-chars (number-of-chars stream)
  (loop with chars := '()
	repeat number-of-chars
	for char := (read-char stream nil nil)
	while char
	do (push char chars)
	finally (let ((read-chars (nreverse (copy-list chars))))
		  (dotimes (j (length chars))
		    (unread-char (pop chars) stream))
		  (return read-chars))))

(defun process-weaver-commands (docsystem line stream)
  (let ((command-prefix `(#\( ,(read-config :command-prefix))))
    (with-input-from-string (s line)
      (loop
	while (peek-char nil s nil nil)
	do
	(let ((chars (peek-n-chars 2 s)))
	  (if (equalp chars command-prefix)
	      ;; a command invocation was found, use CL:READ function to read it
	      (let ((command (read s)))
		(destructuring-bind (command-name &rest args) command
		  (process-weaver-command docsystem 
		   (intern (subseq (symbol-name command-name) 1) :keyword)
		   args stream)))
	      ;; else, continue
	      (write-char (read-char s) stream)))))))

(defgeneric process-weaver-command (docsystem command args stream))

(defmacro def-weaver-command-handler (command-name args (&key docsystem) &body body)
  `(defmethod process-weaver-command ((docsystem ,(or docsystem 'T))
				      (command (eql ,(intern (symbol-name command-name) :keyword)))
				      args stream)
     (destructuring-bind ,args args
       ,@body)))					       

;; @clpackage-functions: Produce a Texinfo section with a package external function definitions.
;; @clpackage-variables: Same with variables.
;; @clpackage-classes: Same with classes.

;; TODO: make @cldefun reference source code when enabled (use swank location).
;; Source code enabled is indicated with a Texinfo variable.
;; Source code is serialized to a Texinfo node with an anchor for each line @anchor{<filename>L<linename>}


;; an idea could be to use the following via an @clpackage-reference{package-name} macro that expands to a full Texinfo chapter with definitions
(defun generate-texinfo-reference-for-package (package stream)
  "Generates a Texinfo reference with PACKAGE external symbols documentation")

(defvar *config* nil
  "The current weaver configuration")

(defvar +default-config+ (list :docsystem :texinfo
			       :parse-docstrings t
			       :command-prefix #\@))

(defun read-config (key)
  "Reads KEY in current *CONFIG*. Returns default in +DEFAULT-CONFIG+ if KEY is not set."
  (if (find key *config*)
      (getf *config* key)
      (getf +default-config+ key)))

(defun weave-file (file output-file &rest options &key docsystem modules command-prefix (parse-docstrings t))
  "Weaves documentation source in FILE and writes the result to OUTPUT-FILE.

Arguments:

- DOCSYSTEM: specify the documentation tool that is being used (:texinfo, :markdown, etc.).
- MODULES: is the list of modules (or ASDF system names) that need to be loaded to be able to read definition descriptions.
- COMMAND-PREFIX: is the character to use as prefix for commands. The character `at` is the default.
- PARSE-DOCSTRINGS: if T, then docstings are parsed and highlighted and references to code from it created."
  (loop for module-name in modules do (require module-name))
  (with-open-file (output output-file :direction :output
                                      :external-format :utf-8
                                      :if-does-not-exist :create
                                      :if-exists :supersede)
    (let ((*config* options))
      (loop for module-name in (getf *config* :modules)
	    do (require module-name))      
      (docsystem-weave-file (or docsystem (read-config :docsystem)) file output))))

(defmethod process-weaver-command (docsystem (command (eql :setup)) args stream)
  (setf *config* args))

(provide :docweaver)
