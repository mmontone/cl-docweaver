.PHONY: all clean

all: cl-docweaver.info cl-docweaver.html index.html cl-docweaver-internals.html cl-docweaver.pdf ../README.md

cl-docweaver.info: cl-docweaver.weaved.texi
	makeinfo --no-validate cl-docweaver.weaved.texi

cl-docweaver.pdf: cl-docweaver.weaved.texi
	texi2any --force --no-validate --pdf cl-docweaver.weaved.texi -o cl-docweaver.pdf

cl-docweaver.html: cl-docweaver.weaved.texi
	texi2any --no-validate --html cl-docweaver.weaved.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o cl-docweaver.html
	sbcl --no-sysinit --no-userinit --load colorize-lisp-examples.lisp cl-docweaver.html

index.html: cl-docweaver.weaved.texi
	texi2any --no-validate --html cl-docweaver.weaved.texi --css-ref texinfo-klare.css --css-ref lisp-coloring.css --no-split -o index.html
	sbcl --no-sysinit --no-userinit --load colorize-lisp-examples.lisp index.html

cl-docweaver.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-docweaver.texi" "cl-docweaver.weaved.texi" :docsystem :texinfo)' --quit


cl-docweaver-internals.info: cl-docweaver-internals.weaved.texi
	makeinfo --no-validate cl-docweaver-internals.weaved.texi

cl-docweaver-internals.pdf: cl-docweaver-internals.weaved.texi
	texi2any --force --no-validate --pdf cl-docweaver-internals.weaved.texi -o cl-docweaver-internals.pdf

cl-docweaver-internals.html: cl-docweaver-internals.weaved.texi
	texi2any --no-validate --html cl-docweaver-internals.weaved.texi --css-ref texinfo-klare.css --no-split -o cl-docweaver-internals.html

cl-docweaver-internals.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-docweaver-internals.texi" "cl-docweaver-internals.weaved.texi" :docsystem :texinfo)' --quit


../README.md:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "README.md" "../README.md" :docsystem :markdown)' --quit

clean:
	rm -f *.weaved*
	rm -f *.pdf
	rm -f *.html
	rm -f ../README.md
