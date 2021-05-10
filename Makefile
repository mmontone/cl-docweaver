.PHONY: docs

cl-docweaver:
	sbcl --load cli.lisp
clean:
	rm ./cl-docweaver
docs:
	cd docs; make
