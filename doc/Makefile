.PHONY: all clean

all: cl-docweaver.html cl-docweaver.pdf

cl-docweaver.pdf: cl-docweaver.weaved.texi
	texi2any --pdf cl-docweaver.weaved.texi -o cl-docweaver.pdf

cl-docweaver.html: cl-docweaver.weaved.texi
	texi2any --html cl-docweaver.weaved.texi --no-split -o cl-docweaver.html

cl-docweaver.weaved.texi:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "cl-docweaver.texi" "cl-docweaver.weaved.texi" :backend :texinfo)' --quit

../README.md:
	sbcl --eval '(ql:quickload :docweaver)' --eval '(docweaver:weave-file "README.md" "../README.md" :backend :markdown)' --quit

clean:
	rm -f cl-docweaver.weaved.texi
	rm -f *.pdf
	rm -f *.html
	rm -f ../README.md
