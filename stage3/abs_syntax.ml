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


(** tipos *)
type typ = Int | Void


(** declaracao de variavel *)
type vardecl = typ * id


(** operadores unarios *)
type unop = UMinus | Not


(** operadores binarios *)
type binop = Plus | Minus | Mult | Div | Lt | Gt | And | Eq


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
  | PrintS of expr
  | PrintI of expr
  | Attrib of id * expr
  | Block of stmt list


(** declaracao de funcao *)
type fundecl = (typ * id * vardecl list * vardecl list * stmt list * expr)


(** programa *)
type prog = fundecl list




