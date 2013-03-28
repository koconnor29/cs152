type token =
  | INT of (int)
  | VAR of (string)
  | PLUS
  | MINUS
  | TIMES
  | EOF
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | EQUALS
  | LESS
  | LESSEQ
  | NOT
  | AND
  | OR
  | SKIP
  | ASSIGN
  | SEMI
  | IF
  | THEN
  | ELSE
  | WHILE
  | DO
  | LBRACE
  | RBRACE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.com
