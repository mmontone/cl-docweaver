.PHONY: docs

cl-docweaver:
	sbcl --load cli.lisp
install:
	cp ./cl-docweaver /usr/local/bin
clean:
	rm ./cl-docweaver
docs:
	cd docs; make
