(defpackage :docweaver/utils
  (:use :cl)
  (:export :symbols-matching
   :symbols-categorized))

(in-package :docweaver/utils)

(defun symbols-matching (package substring definition-type)
  "Returns the list of symbosl in PACKAGE which name have SUBSTRING as substring, and the definition is of type DEFINITION-TYPE."
  (let ((predicate (ecase definition-type
		     (:function 'def-properties:symbol-function-p)
		     (:variable 'def-properties:symbol-variable-p))))
    (let ((symbols
	    (remove-if-not predicate (docweaver::package-symbols (find-package package)))))
      (remove-if-not (lambda (symbol)
		       (search substring (symbol-name symbol) :test 'equalp))
		     symbols))))

;;(symbols-matching :cl "FILE" :function)
;;(symbols-matching :cl "PATHNAME" :function)

(defun symbols-categorized (package category definition-type)
  (let ((predicate (ecase definition-type
		     (:function 'def-properties:symbol-function-p)
		     (:variable 'def-properties:symbol-variable-p))))
    (let ((symbols
	    (remove-if-not predicate (docweaver::package-symbols (find-package package))))
	  (category-regex (format nil "Category:\\s*~a" category)))
      (remove-if-not (lambda (symbol)
		       (ppcre:scan category-regex
				   (documentation symbol (ecase definition-type
							   (:function 'function)
							   (:variable 'variable)))))
		     symbols))))

(defun foo ()
  "FOO function.

Category: foobar."
  'foo)

(defun bar ()
  "BAR function.

Category: foobar."
  'bar)

;;(symbols-categorized :docweaver/utils "foobar" :function)
