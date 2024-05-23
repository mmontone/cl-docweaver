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

After that, `cl-docweaver` command is available: 
```
cl-docweaver - Common Lisp Documentation Weaver

USAGE: sbcl [OPTIONS] INPUTFILE

Weave Common Lisp documentation in INPUTFILE.

Options:
  --version             display version information and exit
  -h, --help            display help information and exit
  -d, --debug           debug
  -o FILE, --output FILE
                        output file
  -s DOCSYSTEM, --docsystem DOCSYSTEM
                        the documentation system to use. either texinfo or
                        markdown. if not specified, the documentation system
                        used is inferred by looking at input file extension.
  -m MODULES, --modules MODULES
                        the list of modules to REQUIRE
  -c COMMAND-PREFIX, --command-prefix COMMAND-PREFIX
                        the command prefix character to use. Default is @
  --parse-docstrings    When enabled, parse docstrings and format them. This is
                        enabled by default.
  --escape-docstrings   When enabled, escape the docstrings depending on the
                        output. This is enabled by default.

Examples:

  Weave texinfo file and visualize weaved output

      cl-docweaver my-documentation.texi

  Weave texinfo file into a file

      cl-docweaver my-documentation.texi -o my-documentation.weaved.texi

```

## API

## DOCWEAVER

- [function] **DOCWEAVER:WEAVE-FILE** *(file output-file &key docsystem modules command-prefix (parse-docstrings t) (escape-docstrings t))*

    Weaves documentation source in FILE and writes the result to OUTPUT-FILE.
    
    Arguments:
    
    - DOCSYSTEM : specify the documentation tool that is being used (:texinfo, :markdown, etc.).
    - MODULES : is the list of modules (or ASDF system names) that need to be loaded to be able to read definition descriptions.
    - COMMAND-PREFIX : is the character to use as prefix for commands. The character `at` is the default.
    - PARSE-DOCSTRINGS : if T, then docstrings are parsed and highlighted and references to code from it created.
    - ESCAPE-DOCSTRINGS: if T, then docstrings are escaped by the documentation system. Escaping allows the use of special documentation system characters in docstring sources. If the escaping of docstrings is turned off, then that allows to use documentation system markup in docstrings.
    Category: TopLevel



- [function] **DOCWEAVER:DEF-WEAVER-COMMAND-HANDLER** *(command-name args (&key docsystem) &body body)*

    Define a weaver command handler.
    COMMAND-NAME is the name of the command, without the prefix (like 'clvariable', 'clfunction', etc.)
    ARGS is the list of arguments for that command in the DOCSYSTEM implementation.
    DOCSYSTEM is a specializer for the documentation system. For example, (eql :texinfo).
    BODY should write to an implicit STREAM variable, to expand the command.
    
    This is implemented as a wraper over PROCESS-WEAVER-COMMAND .



- [function] **DOCWEAVER:PROCESS-WEAVER-COMMAND** *(docsystem command args stream)*

    The generic function to specialize for implementing weaving commands for the different documentation systems.
    
    See: DEF-WEAVER-COMMAND-HANDLER




