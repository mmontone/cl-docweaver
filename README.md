

# CL-DOCWEAVER

CL-DOCWEAVER is a document weaver for Common Lisp.

Documentation for a Lisp project can be written using (potentially) any tool choosen by the user (like Texinfo, Markdown, etc). Then, Common Lisp definitions are referenced and embedded from the documentation source using some commands.

CL-DOCWEAVER is easy to extend to support different documentation tools.

Texinfo and Markdown are the ones with best support at this moment.

## Use

Invoke WEAVE-FILEon your documentation source files.

### Commands

#### (@setup &rest args)

Configures the weaver.

Some of the parameters are:

- `:parse-docstrings`: When T, definitions docstrings are parsed and highlighted and references to other definitions are added.
- `:docsystem`: Especify the documentation system to use. This same option can be specified when invoking WEAVE-FILE.
- `:command-prefix`: The command prefix character to use. `@` is the default, but it could be useful to use another one depending on the tools being used and the user needs. For example, Texinfo uses `@` for macros, so it could be better to use some other character for docweaver commands.

#### (@clfunction function-symbol &rest args)

Displays function bound to FUNCTION-SYMBOL.

#### (@clpackage package-name &key include-members)

Displays PACKAGE-NAME package description.

If INCLUDE-MEMBERS is T, then all external package definitions are shown.

## API

## DOCWEAVER

### function DOCWEAVER:DEF-WEAVER-COMMAND-HANDLER (command-name args (&key docsystem) &body body)



### function DOCWEAVER:PROCESS-WEAVER-COMMAND (docsystem command args stream)



### function DOCWEAVER:WEAVE-FILE (file output-file &rest options &key docsystem modules command-prefix
 (parse-docstrings t))
Weaves documentation source in FILE and writes the result to OUTPUT-FILE.

Arguments:
 - DOCSYSTEM specify the documentation tool that is being used (:texinfo, :markdown, etc.).
- MODULES is the list of modules (or ASDF system names) that need to be loaded to be able to read definition descriptions.
- COMMAND-PREFIX is the character to use as prefix for commands. The character `at` is the default.
- PARSE-DOCSTRINGS: if T, then docstings are parsed and highlighted and references to code from it created.

See: DOCWEAVER:PROCESS-WEAVER-COMMAND




