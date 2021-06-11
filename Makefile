.PHONY: docs

cl-docweaver:
	sbcl --load cli.lisp --eval '(docweaver/cli:build)'

install:
	cp ./cl-docweaver /usr/local/bin
	cp ./docs/cl-docweaver.info /usr/local/share/info

clean:
	rm ./cl-docweaver

docs:
	cd docs; make
