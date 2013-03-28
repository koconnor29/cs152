See Homework 1 for instructions.

Description of Files:
---------------------
  - ast.ml
      Defines the datatypes for the abstract syntax trees (ASTs).

  - eval.ml
      The interpreter for the ASTs. Your task is to edit this (and
      only this) file to A lexer and parser for IMP programs.

  - main.ml
      The top level code that parses in an input file, and attempts to
      execute it.

  - lexer.mll
  - parser.mly
      A lexer and parser for IMP programs.

  - pprint.ml
      A pretty printer for the ASTs.

  - test.imp
      A test IMP program.

How to compile:
---------------

  If you have make installed, you can simply type "make" at the
  command line. Otherwise, if you are on a Windows machine, you can
  edit the make.bat file to point to your installation of Ocaml, and
  execute the make.bat file. If neither of these methods work, you
  should execute the commands listed in the make.bat file.

  Successful compilation will produce an executable file called "imp".

How to execute:
---------------

  Run the executable on a test program: "imp test.imp".

  This will parse the file test.imp, build the AST, print it, and
  attempt to evaluate the program.

Notes:
------

  OCaml modes exist for emacs
  (http://caml.inria.fr/pub/docs/u3-ocaml/emacs/index.html) and vim
  (http://www.ocaml.info/vim/ftplugin/ocaml.vim).
