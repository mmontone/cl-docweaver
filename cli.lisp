(require :docweaver)
(require :adopt)
(require :trivial-backtrace)

(defpackage :docweaver/cli
  (:use :cl :docweaver)
  (:export :build))

(in-package :docweaver/cli)

(adopt:define-string *help-text*
  "Weave Common Lisp documentation in INPUTFILE.")

(defparameter *option-version*
  (adopt:make-option 'version
                     :long "version"
                     :help "display version information and exit"
                     :reduce (constantly t)))

(defparameter *option-help*
  (adopt:make-option 'help
                     :long "help"
                     :short #\h
                     :help "display help information and exit"
                     :reduce (constantly t)))

(defparameter *option-debug*
  (adopt:make-option 'debug
                     :long "debug"
                     :short #\d
                     :help "debug"
                     :reduce (constantly t)))

(defparameter *option-output*
  (adopt:make-option 'output
                     :long "output"
                     :parameter "FILE"
                     :short #\o
                     :help "output file"
                     :reduce #'adopt:last))

(defparameter *option-docsystem*
  (adopt:make-option 'docsystem
                     :long "docsystem"
                     :parameter "DOCSYSTEM"
                     :short #\s
                     :help (format nil "the documentation system to use. either texinfo or markdown. ~
if not specified, the documentation system used is inferred by looking at input file extension.")
                     :reduce #'adopt:last))

(defparameter *option-modules*
  (adopt:make-option 'modules
		     :long "modules"
		     :parameter "MODULES"
		     :short #\m
		     :help (format nil "the list of modules to REQUIRE")
		     :reduce #'adopt:collect))

(defparameter *option-command-prefix*
  (adopt:make-option 'command-prefix
		     :long "command-prefix"
		     :parameter "COMMAND-PREFIX"
		     :short #\c
		     :help (format nil "the command prefix character to use. Default is @")
		     :reduce #'adopt:last))

(defparameter *option-parse-docstrings*
  (adopt:make-option 'parse-docstrings
                     :long "parse-docstrings"
                     :help "When enabled, parse docstrings and format them. This is enabled by default."
                     :reduce (constantly t)))

(defparameter *option-escape-docstrings*
  (adopt:make-option 'escape-docstrings
                     :long "escape-docstrings"
                     :help "When enabled, escape the docstrings depending on the output. This is enabled by default."
                     :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
   :name "cl-docweaver"
   :summary "Common Lisp Documentation Weaver"
   :usage "[OPTIONS] INPUTFILE"
   :examples '(("Weave texinfo file and visualize weaved output" .
                "cl-docweaver my-documentation.texi")
               ("Weave texinfo file into a file" .
                "cl-docweaver my-documentation.texi -o my-documentation.weaved.texi"))
   :contents (list *option-version* *option-help* *option-debug* *option-output* *option-docsystem* *option-modules* *option-command-prefix* *option-parse-docstrings* *option-escape-docstrings*)
   :help *help-text*))

(defun run (input-file &key output docsystem modules command-prefix debug parse-docstrings escape-docstrings)
  (let (input-pathname output-pathname docsystem-discriminator)

    (setf input-pathname (pathname input-file))
    (when (not (UIOP/PATHNAME:ABSOLUTE-PATHNAME-P input-pathname))
      (setf input-pathname (merge-pathnames input-pathname (uiop/os:getcwd))))

    (when output
      (setf output-pathname (pathname output))
      (when (not (UIOP/PATHNAME:ABSOLUTE-PATHNAME-P output-pathname))
        (setf output-pathname (merge-pathnames output-pathname (uiop/os:getcwd)))))
    (when (not output-pathname)
      (setf output-pathname (merge-pathnames (format nil "~a.weaved.~a"
                                                     (pathname-name input-file)
                                                     (pathname-type input-file))
                                             (uiop/os:getcwd))))
    (when docsystem
      (unless (member docsystem '("texinfo" "markdown") :test 'string=)
        (format t "Invalid documentation system: ~a~%" docsystem)
        (adopt:exit))
      (setf docsystem-discriminator (intern (string-upcase docsystem) :keyword)))

    (when (not docsystem-discriminator)
      (when (member (pathname-type input-file) '("texi" "texinfo") :test 'string=)
        (setf docsystem-discriminator :texinfo))
      (when (member (pathname-type input-file) '("md" "markdown") :test 'string=)
        (setf docsystem-discriminator :markdown))
      (unless docsystem-discriminator
        (format t "Could not infer documentation system to use.~%")
        (adopt:exit)))

    (when debug
      (trace docweaver:weave-file))
    
    (apply #'docweaver:weave-file input-pathname output-pathname

	   `(:docsystem ,docsystem-discriminator
	     :modules ,modules
	     ,@(when command-prefix
		 `(:command-prefix ,(coerce command-prefix 'character)))
	     :parse-docstrings ,parse-docstrings
	     :escape-docstrings ,escape-docstrings))))

(defvar *debug* nil)

(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version options)
          (format t "~a~%"
		  (asdf:component-version (asdf:find-system "docweaver")))
          (adopt:exit))
        (when (zerop (length arguments))
          (adopt:print-help-and-exit *ui*))
        (unless (= 1 (length arguments))
          (format t "Invalid syntax.~%")
          (adopt:exit))
	(setf *debug* (gethash 'debug options))
        (destructuring-bind (input-file) arguments
          (run input-file
               :output (gethash 'output options)
               :docsystem (gethash 'docsystem options)
	       :debug (gethash 'debug options)
	       :modules (gethash 'modules options)
	       :command-prefix (gethash 'command-prefix options)
	       :parse-docstrings (gethash 'parse-docstrings options)
	       :escape-docstrings (gethash 'escape-docstrings options))))
    (error (c)
      (when *debug*
	(trivial-backtrace:print-backtrace c))
      (adopt:print-error-and-exit c))))

(defun build ()
  (sb-ext:save-lisp-and-die "cl-docweaver"
                            :save-runtime-options t
                            :executable t
                            :toplevel #'toplevel))
