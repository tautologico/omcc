(* 
 *
 * Compilador MiniC em OCaml
 * 
 * Gerador de Codigo usando LLVM como alvo. 
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 *)

open Llvm
open Abs_syntax

(** Tipo para tabelas de simbolos de variaveis e funcoes *)
type sym_table = (string, llvalue) Hashtbl.t

(** Contexto global para a LLVM *)
let llcontext = global_context ()

(** Modulo para abrigar o codigo gerado *)
let the_module = create_module llcontext "MiniC_Module"

(** builder para insercao de instrucoes *)
let builder = builder llcontext

(** Tabela de funcoes *)
(* let function_table: sym_table = Hashtbl.create 10 *)

(** Tipo para inteiros com tamanho de palavra nativo *)
let int_type = match Sys.word_size with
  | 32 -> (i32_type llcontext)
  | 64 -> (i64_type llcontext)
  | _ -> failwith "Nao foi possivel determinar o tamanho da palavra"

(** Constante 0 *)
let zero = const_int int_type 0

(** Constante 1 *)
let one = const_int int_type 1


(** Obtem uma funcao ja compilada para bitcode *)
let get_bc_function fname = match lookup_function fname the_module with
    Some f -> f
  | None -> failwith "Funcao nao encontrada"

(** Adiciona parametros na tabela de simbolos e 
    associa-os a nomes no codigo LLVM *)
let add_parms (symtbl: sym_table) pdecls parms = 
  let add_var v p = 
    if Hashtbl.mem symtbl v then
      failwith ("Variavel declarada mais de uma vez: " ^ v)
    else
      set_value_name v p; 
      Hashtbl.add symtbl v p in
  List.iter2 add_var (List.map snd pdecls) parms

(** Adiciona declaracoes das variaveis locais *)
let add_vars = 0

(** *)
let translate_fundef (t, name, parms, lvars, stmts, retexp) =
  let parms_type = Array.make (List.length parms) int_type in
  let ft = function_type int_type parms_type in
  let f = match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some _ -> failwith ("Funcao definida mais de uma vez: " ^ name) in
  let stbl = Hashtbl.create 10 in
    add_parms stbl parms (Array.to_list (params f))


(** Traduz uma expressao *)
let rec translate_expr (symtbl: sym_table) e = match e with
    Num n    -> const_int int_type n
  | String s -> const_stringz llcontext s 
  | BinOp (el, op, er) -> let lop = translate_expr symtbl el in
                          let rop = translate_expr symtbl er in
			  translate_binop op lop rop
  | UnOp (op, e) -> let rand = translate_expr symtbl e in
                    translate_unop op rand
  | Var v -> (try Hashtbl.find symtbl v with 
		  Not_found -> failwith ("Variavel nao definida: " ^ v))
  | FunCall (fn, args) -> 
      let f = get_bc_function fn in
      let parms = params f in 
      if Array.length parms = List.length args then
	let args = Array.of_list (List.map (translate_expr symtbl) args) in
        build_call f args "calltmp" builder
      else
	failwith ("Numero de argumentos incorreto para funcao " ^ fn)
and translate_binop op lop rop = match op with
    Plus  -> build_add  lop rop "addtmp" builder
  | Minus -> build_sub  lop rop "subtmp" builder
  | Mult  -> build_mul  lop rop "multmp" builder
  | Div   -> build_sdiv lop rop "divtmp" builder
  | And   -> build_and  lop rop "andtmp" builder
  | Lt    -> build_icmp Icmp.Slt lop rop "lttmp" builder
  | Eq    -> build_icmp Icmp.Eq lop rop "eqtmp" builder
and translate_unop op rand = match op with
    UMinus -> build_sub zero rand "negtmp" builder   (* -x = 0 - x *)
  | Not    -> build_sub one rand "nottmp" builder    (* !x = 1 - x *)

(* let rec gen_code node = match node with *)
(*   |  *)
