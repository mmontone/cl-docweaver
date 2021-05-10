(require :docweaver)
(require :adopt)

(defpackage :docweaver/cli
  (:use :cl :docweaver))

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

(defparameter *ui*
  (adopt:make-interface
   :name "cl-docweaver"
   :summary "Common Lisp Documentation Weaver"
   :usage "[OPTIONS] INPUTFILE"
   :examples '(("Weave texinfo file and visualize weaved output" .
                "cl-docweaver my-documentation.texi")
               ("Weave texinfo file into a file" .
                "cl-docweaver my-documentation.texi -o my-documentation.weaved.texi"))
   :contents (list *option-version* *option-help* *option-output* *option-docsystem*)
   :help *help-text*))

(defun run (input-file &key output docsystem)
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

    (docweaver:weave-file input-pathname output-pathname
                          :docsystem docsystem-discriminator)))


(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when (gethash 'version options)
          (format t "1.0.0~%")
          (adopt:exit))
        (when (zerop (length arguments))
          (adopt:print-help-and-exit *ui*))
        (unless (= 1 (length arguments))
          (format t "Invalid syntax.~%")
          (adopt:exit))
        (destructuring-bind (input-file) arguments
          (run input-file
               :output (gethash 'output options)
               :docsystem (gethash 'docsystem options))))
    (error (c)
      (adopt:print-error-and-exit c))))

(defun build ()
  (sb-ext:save-lisp-and-die "cl-docweaver"
                            :save-runtime-options t
                            :executable t
                            :toplevel #'toplevel))

(build)
