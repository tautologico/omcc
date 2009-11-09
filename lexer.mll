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
}

(*** definicoes auxiliares ***)
let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z' '_']

let id = letter (letter | digit)*

let whitespc = [' ' '\r' '\n' '\t']
let linecmt = "//" [^ '\n']*


(*** regras ***)
rule minic_lexer = parser
    digit+ as inum          { NUM(int_of_string inum) }    (* literais numericos *)

  | "#include <stdio.h>"	{ PROLOG }

  (* palavras-chave *)
  | "char"			{ CHAR   }
  | "else"			{ ELSE   }
  | "if"			{ IF     }
  | "int"			{ INT    }
  | "main"			{ MAIN   }
  | "printf"			{ PRINTF }
  | "return"			{ RETURN }
  | "void"			{ VOID   }
  | "while"			{ WHILE  }

  (* operadores *)
  | '+'				{ PLUS   }
  | '-'				{ MINUS  }
  | '*'				{ MULT   }
  | '/'				{ DIV    }
  | '<'				{ LT     }
  | "&&"			{ AND    }
  | '='				{ ATTRIB }
  | "=="			{ EQ     }

  (* pontuacao *)
  | '{'				{ LBRACE }
  | '}'				{ RBRACE }
  | ';'				{ SEMICOLON }
  | '('				{ LPAREN }
  | ')'				{ RPAREN }


  | id as text			{ ID(text)  }           (* identificadores *)

  | whitespc			{ minic_lexer lexbuf } 
  | linecmt			{ minic_lexer lexbuf }
