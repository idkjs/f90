
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | TO
  | THEN
  | S_RPAREN
  | SUBROUTINE
  | STOP
  | SELECT
  | RPAREN
  | RETURN
  | REAL
  | RBRACE
  | PROGRAM
  | PRECISION
  | POINTER
  | PLUS
  | PARAMETER
  | OR
  | NOT
  | NEQV
  | NEQ
  | MUL
  | MINUS
  | LPAREN_S
  | LPAREN
  | LOGICAL
  | LESS
  | LEQ
  | LBRACE
  | INTEGER
  | INT of (int)
  | IF
  | IDENT of (string)
  | GREATER
  | GOTO
  | GO
  | GEQ
  | FUNCTION
  | FLOAT of (string)
  | FALSE
  | EQV
  | EQEQ
  | EQ
  | EOF
  | END
  | ELSE
  | DOUBLE
  | DO
  | DIV
  | DIMENSION
  | DEFAULT
  | CONTAINS
  | COMPLEX
  | COMMENT
  | COMMA
  | COLON
  | COLCOL
  | CASE
  | CALL
  | BR
  | AND
  | ALLOCATABLE

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (unit Parse_tree.main)
