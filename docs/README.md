# CL-DOCWEAVER

CL-DOCWEAVER is a document weaver for Common Lisp.

Documentation for a Lisp project is written with the user's tool of choice (like Texinfo, Markdown, etc). Then, Common Lisp definitions are expanded into the documentation source using DocWeaver commands.

DOCWEAVER commands give the user control on how definitions are to be expanded, either via command options or by choosing a different set of commands.

CL-DOCWEAVER is easy to extend to support different documentation tools.

Texinfo and Markdown are the ones with best support at this moment.

## Usage

Please have a look at the [manual](docs/cl-docweaver.pdf "manual").

### cl-docweaver command line command

Build the cl-docweaver command line command:

```
make
sudo make install
```

After that, `cl-docweaver` command is available: ($cleval (progn (load "../cli.lisp") ""))
```
($clcapture-output (adopt:print-help docweaver/cli::*ui*))
```

## API

($clpackage :docweaver :include-external-definitions t)
