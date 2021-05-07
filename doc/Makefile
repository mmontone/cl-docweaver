.PHONY: all clean

all: cl-docweaver.info cl-docweaver.html cl-docweaver.pdf ../README.md

cl-docweaver.info: cl-docweaver.weaved.texi
	makeinfo --no-validate cl-docweaver.weaved.texi

cl-docweaver.pdf: cl-docweaver.weaved.texi
# TODO: FIX texi2any --no-validate --pdf cl-docweaver.weaved.texi -o cl-docweaver.pdf

cl-docweaver.html: cl-docweaver.weaved.texi
	texi2any --no-validate --html cl-docweaver.weaved.texi --no-split -o cl-docweaver.html

cl-docweaver.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-docweaver.texi" "cl-docweaver.weaved.texi" :docsystem :texinfo)' --quit

../README.md:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "README.md" "../README.md" :docsystem :markdown)' --quit

clean:
	rm -f cl-docweaver.weaved.texi
	rm -f *.pdf
	rm -f *.html
	rm -f ../README.md
