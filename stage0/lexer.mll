(* 
 *
 * Compilador MiniC em OCaml
 * 
 * Especificacao Lexica para a linguagem MiniC, usando ocamllex
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 *)

(*** codigo inicial ***)
{
  open Parser

  let remove_quotes s = String.sub s 1 (String.length s - 2)
}

(*** definicoes auxiliares ***)
let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z' '_']

let id = letter (letter | digit)*

let whitespc = [' ' '\r' '\n' '\t']
let linecmt = "//" [^ '\n']*


(*** regras ***)
rule next_token = parse
    digit+ as inum          { NUM(int_of_string inum) }    (* literais numericos *)

  | "#include <stdio.h>"	{ PROLOG }

  | '"' [^'"' '\n']* '"' as s   { STR (remove_quotes s) }

  (* palavras-chave *)
  | "int"			{ INT      }
  | "printf"			{ PRINTF   }
  | "printint"                  { PRINTINT }
  | "return"			{ RETURN   }


  (* operadores *)
  | '+'				{ PLUS   }
  | '-'				{ MINUS  }
  | '*'				{ MULT   }
  | '/'				{ DIV    }
  | '<'				{ LT     }
  | "&&"			{ AND    }
  | "=="			{ EQ     }
  | '='				{ ATTRIB }
  | '!'                         { NOT    }

  (* pontuacao *)
  | '{'				{ LBRACE }
  | '}'				{ RBRACE }
  | ';'				{ SEMICOLON }
  | '('				{ LPAREN }
  | ')'				{ RPAREN }
  | ','                         { COMMA  }


  | id as text			{ ID(text)  }           (* identificadores *)

  | whitespc			{ next_token lexbuf } 
  | linecmt			{ next_token lexbuf }

  | eof                         { EOF }
