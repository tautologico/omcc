(* 
 *
 * Compilador MiniC em OCaml
 * 
 * Sintaxe Abstrata para a linguagem MiniC
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 *)


(** identificadores *)
type id = string


(** programa *)
type prog = fundecl list


(** declaracao de funcao *)
type fundecl = (typ * id * parm list * vardecl list * stmt list * expr)


(** tipos *)
type typ = Int | Void


(** operadores unarios *)
type unop = UPlus | UMinus | Not


(** operadores binarios *)
type binop = Plus | Minus | Mult | Div | Lt | And


(** expressoes *)
type expr = 
    Num of int 
  | String of string
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | Var of id
  | FunCall of id * expr list


(** comandos *)
type stmt = 
    If of expr * stmt * stmt option
  | While of expr * stmt
  | Print of expr
  | Attrib of id * expr
  | Block of stmt list


