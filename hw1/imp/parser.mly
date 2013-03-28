%{
  open Ast
  open Printf

  open Lexing
%}

%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES EOF 
  LPAREN RPAREN TRUE FALSE EQUALS LESS LESSEQ NOT AND OR 
  SKIP ASSIGN SEMI IF THEN ELSE WHILE DO LBRACE RBRACE

%type <com> c 
%type <aexp> a
%type <bexp> b
%type <Ast.com> program

%left SEMI
%nonassoc EQUALS
%left LESS LESSEQ
%left OR PLUS MINUS
%left AND TIMES
%right NOT

%start program
 
%%

a : INT              { Int $1 }
  | VAR              { Var $1 }
  | a PLUS a         { Plus($1, $3) }
  | a MINUS a        { Minus($1, $3) }
  | a TIMES a        { Times($1, $3) }
  | LPAREN a RPAREN  { $2 }

b : TRUE             { True }
  | FALSE            { False }
  | a EQUALS a       { Equals($1, $3) }
  | a LESS a         { Less($1, $3) }
  | a LESSEQ a       { LessEq($1, $3) }
  | LPAREN b RPAREN  { $2 }

c : SKIP             { Skip }
  | VAR ASSIGN a     { Assign($1, $3) }
  | c SEMI c         { Seq($1, $3) }
  | IF LPAREN b RPAREN THEN LBRACE c RBRACE ELSE LBRACE c RBRACE   
                     { If($3, $7, $11) }
  | WHILE LPAREN b RPAREN DO LBRACE c RBRACE     
                     { While($3, $7) }
    
program : c          { $1 }
