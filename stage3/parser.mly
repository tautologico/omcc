/* 
 *
 * Compilador MiniC em OCaml
 * 
 * Especificacao Sintatica para a linguagem MiniC, usando ocamlyacc
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 */

%{
  open Abs_syntax

  let parse_error s = print_endline s
%}


/* prologo */
%token PROLOG

/* palavras-chave */
%token ELSE IF INT PRINTF PRINTINT RETURN WHILE 

/* operadores */
%token PLUS MINUS MULT DIV LT GT AND ATTRIB EQ NOT

/* pontuacao */
%token LBRACE RBRACE SEMICOLON LPAREN RPAREN COMMA

%token EOF

%token <int> NUM
%token <string> ID
%token <string> STR


%start program
%type <Abs_syntax.prog> program



%%

program: prolog fundecls EOF { $2 }

prolog: PROLOG               {    }

fundecls:                    { []       }
        | fundecl fundecls   { $1 :: $2 }

fundecl: rtype ID LPAREN arglist RPAREN
         LBRACE 
           vardecls stmts RETURN expr SEMICOLON
         RBRACE               { ($1, $2, $4, $7, $8, $10) }


rtype: INT                    { Int  }


arglist:                      { []              }
       | INT ID restarglst    { (Int, $2) :: $3 }

restarglst:                   { [] }
          | COMMA arglist     { $2 }


vardecls:                     { []        }
	| vardecl vardecls    { $1 :: $2  }

vardecl: INT ID SEMICOLON     { (Int, $2) }

stmts:                        { []       }
     | stmt stmts             { $1 :: $2 }

stmt: IF LPAREN expr RPAREN stmt            { If ($3, $5, None)    }
    | IF LPAREN expr RPAREN stmt ELSE stmt  { If ($3, $5, Some $7) }
    | WHILE LPAREN expr RPAREN stmt         { While ($3, $5)       }
    | PRINTF LPAREN expr RPAREN SEMICOLON   { PrintS $3            }
    | PRINTINT LPAREN expr RPAREN SEMICOLON { PrintI $3            }
    | ID ATTRIB expr SEMICOLON              { Attrib ($1, $3)      }
    | LBRACE stmts RBRACE                   { Block $2             }

expr: andexpr                 { $1 }

andexpr: andexpr AND relexpr   { BinOp ($1, And, $3)   }
       | relexpr               { $1                    }

relexpr: relexpr LT addexpr    { BinOp ($1, Lt, $3)    }
       | relexpr GT addexpr    { BinOp ($1, Gt, $3)    }
       | relexpr EQ addexpr    { BinOp ($1, Eq, $3)    }
       | addexpr               { $1                    }

addexpr: addexpr PLUS mulexpr  { BinOp ($1, Plus, $3)  }
       | addexpr MINUS mulexpr { BinOp ($1, Minus, $3) }
       | mulexpr               { $1                    }

mulexpr: mulexpr MULT unexp    { BinOp ($1, Mult, $3)  }
       | mulexpr DIV unexp     { BinOp ($1, Div, $3)   }
       | unexp                 { $1                    }

unexp: ID                       { Var $1            }
     | NUM                      { Num $1            }
     | STR                      { String $1         }
     | LPAREN expr RPAREN       { $2                }
     | NOT expr                 { UnOp (Not, $2)    }
     | MINUS expr               { UnOp (UMinus, $2) }
     | ID LPAREN explist RPAREN { FunCall ($1, $3)  }

explist:                        { []       }
       | expr rexplist          { $1 :: $2 }

rexplist:                       { [] }
        | COMMA explist         { $2 }

%%

