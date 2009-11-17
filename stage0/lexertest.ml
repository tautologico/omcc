(* 
 *
 * Compilador MiniC em OCaml
 * 
 * Teste do Analisador Lexico para MiniC
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 *)

open Parser

let tok2str tok = match tok with
  | PROLOG     -> "PROLOG"
  | CHAR       -> "CHAR"
  | ELSE       -> "ELSE"
  | IF         -> "IF"
  | INT        -> "INT" 
  | PRINTF     -> "PRINTF"
  | RETURN     -> "RETURN"
  | WHILE      -> "WHILE"
  | PLUS       -> "PLUS"
  | MINUS      -> "MINUS"
  | MULT       -> "MULT"
  | DIV        -> "DIV"
  | LT         -> "LT"
  | AND        -> "AND"
  | ATTRIB     -> "ATTRIB"
  | EQ         -> "EQ"
  | NOT        -> "NOT"
  | LBRACE     -> "LBRACE"
  | RBRACE     -> "RBRACE"
  | SEMICOLON  -> "SEMICOLON"
  | LPAREN     -> "LPAREN"
  | RPAREN     -> "RPAREN"
  | COMMA      -> "COMMA" 
  | EOF        -> "EOF"
  | NUM n      -> "NUM " ^ (string_of_int n)
  | ID id      -> "ID " ^ id
  | STR s      -> "STR '" ^ s ^ "'"

let main fname = 
  let infile = open_in fname in
  let lexbuf = Lexing.from_channel infile in
  while true do
    let tok = Lexer.next_token lexbuf in
    print_endline (tok2str tok)
  done

let _ = 
  if Array.length Sys.argv < 2 then
    print_endline "Especifique o arquivo de entrada"
  else
    main Sys.argv.(1)

