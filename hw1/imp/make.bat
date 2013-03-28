set top=c:\ocaml
set ocamlc=%top%\ocamlc
set ocamllex=%top%\ocamllex
set ocamlyacc=%top%\ocamlyacc

%ocamlc% -c ast.ml
%ocamlyacc% -q parser.mly
%ocamlc% -c parser.mli
%ocamllex% -q lexer.mll
%ocamlc% -c lexer.ml
%ocamlc% -c parser.ml
%ocamlc% -c pprint.ml
%ocamlc% -c eval.ml
%ocamlc% -c main.ml
%ocamlc% -o imp ast.cmo lexer.cmo parser.cmo pprint.cmo eval.cmo main.cmo

