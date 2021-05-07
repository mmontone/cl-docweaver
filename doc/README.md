(@setup :command-prefix #\$ :docsystem :markdown)

# CL-DOCWEAVER

CL-DOCWEAVER is a document weaver for Common Lisp.

Documentation for a Lisp project can be written using (potentially) any tool choosen by the user (like Texinfo, Markdown, etc). Then, Common Lisp definitions are referenced and embedded from the documentation source using weaving commands.

CL-DOCWEAVER is easy to extend to support different documentation tools.

Texinfo and Markdown are the ones with best support at this moment.

THIS IS WORK IN PROGRESS.

## Use

Plase have a look at the [manual](doc/cl-docweaver.html "manual").

## API

($clpackage :docweaver :include-external-definitions t)
