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
  let parser_error s = print_endline s
%}


/* prologo */
%token PROLOG

/* palavras-chave */
%token CHAR ELSE IF INT MAIN PRINTF RETURN VOID WHILE 

/* operadores */
%token PLUS MINUS MULT DIV LT AND ATTRIB EQ

/* pontuacao */
%token LBRACE RBRACE SEMICOLON LPAREN RPAREN

%token <int> NUM
%token <string> ID



%%


%%

