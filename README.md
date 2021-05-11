

# CL-DOCWEAVER

CL-DOCWEAVER is a document weaver for Common Lisp.

Documentation for a Lisp project is written with the user's tool of choice (like Texinfo, Markdown, etc). Then, Common Lisp definitions are expanded into the documentation source using DocWeaver commands.

DOCWEAVER commands give the user control on how definitions are to be expanded, either via command options or by choosing a different set of commands.

CL-DOCWEAVER is easy to extend to support different documentation tools.

Texinfo and Markdown are the ones with best support at this moment.

## Usage

Plase have a look at the [manual](docs/cl-docweaver.pdf "manual").

## API

## DOCWEAVER

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



- [function] **DOCWEAVER:WEAVE-FILE** *(file output-file &rest options &key docsystem modules command-prefix (parse-docstrings t))*

    Weaves documentation source in FILE and writes the result to OUTPUT-FILE.
    
    Arguments:
    
    - DOCSYSTEM : specify the documentation tool that is being used (:texinfo, :markdown, etc.).
    - MODULES : is the list of modules (or ASDF system names) that need to be loaded to be able to read definition descriptions.
    - COMMAND-PREFIX : is the character to use as prefix for commands. The character `at` is the default.
    - PARSE-DOCSTRINGS : if T, then docstings are parsed and highlighted and references to code from it created.




