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

(** Tipo para strings (ponteiros para char) *)
let str_type = pointer_type (i8_type llcontext)

(** Buffer para conversao de numeros para string (tamanho 20 caracteres) *)
let itoa_buffer_size = 20

let itoa_buffer = 
  define_global "itoa_buffer" 
    (const_stringz llcontext (String.make itoa_buffer_size '\000')) the_module


(** Referencia para funcoes externas *)
let puts = 
  declare_function "puts" 
    (function_type int_type [| str_type |]) the_module

let snprintf = 
  declare_function "snprintf"
    (var_arg_function_type int_type [| str_type; int_type; str_type |]) the_module

let format_str = define_global "format_str" (const_stringz llcontext "%d") the_module


(** Obtem uma funcao ja compilada para bitcode *)
let get_bc_function fname = match lookup_function fname the_module with
    Some f -> f
  | None -> failwith "Funcao nao encontrada"


let find_variable symtbl name = 
  try Hashtbl.find symtbl name with 
      Not_found -> failwith ("Variavel nao declarada: " ^ name)

(** Traduz uma expressao *)
let rec translate_expr (symtbl: sym_table) e = match e with
    Num n    -> const_int int_type n
  | String s -> let c = const_stringz llcontext s in
                define_global "string_const" c the_module
  | BinOp (el, op, er) -> let lop = translate_expr symtbl el in
                          let rop = translate_expr symtbl er in
			  translate_binop op lop rop
  | UnOp (op, e) -> let rand = translate_expr symtbl e in
                    translate_unop op rand
  | Var v -> let vr = find_variable symtbl v in
             build_load vr v builder
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


(** Verifica se uma expressao e' uma string *)
let check_string e = match e with
    String _ -> true
  | _        -> false

(** Traduz um comando *)
let rec translate_stmt (symtbl: sym_table) s = match s with
    PrintS e -> 
      if check_string e then 
	let v = translate_expr symtbl e in
	let ptr = build_gep v [| zero; zero |] "strtmp" builder in
	  ignore (build_call puts [| ptr |] "" builder)
      else
	failwith "printf deve imprimir apenas strings"
  | PrintI e -> 
      let v = translate_expr symtbl e in
	if type_of v == int_type then
	  let buffer_ptr = build_gep itoa_buffer [| zero; zero |] "bufferptr" builder in
	  let format_ptr = build_gep format_str [| zero; zero |] "formatptr" builder in
	  let size = const_int int_type itoa_buffer_size in
	  let _ = build_call snprintf 
                        [| buffer_ptr; size; format_ptr; v |] "" builder in
	    ignore (build_call puts [| buffer_ptr |] "" builder)
	else
	  failwith "printint deve imprimir inteiros"
  | Attrib (v, e) -> 
      let nv = translate_expr symtbl e in
      let va = find_variable symtbl v in
        ignore (build_store nv va builder)
  | Block sl -> ignore (List.map (translate_stmt symtbl) sl)


(** Cria uma declaracao de variavel de pilha (variavel local)
    no bloco de entrada da funcao f para a variavel v *)
let create_entry_block_alloca f v = 
  let entry_bldr = builder_at llcontext (instr_begin (entry_block f)) in
  build_alloca int_type v entry_bldr

(** Adiciona parametros na tabela de simbolos e 
    associa-os a nomes no codigo LLVM *)
let add_parms f (symtbl: sym_table) pdecls parms = 
  let add_var v p = 
    if Hashtbl.mem symtbl v then
      failwith ("Variavel declarada mais de uma vez: " ^ v)
    else
      let alloca = create_entry_block_alloca f v in
      let _ = build_store p alloca builder in
      Hashtbl.add symtbl v alloca in
  List.iter2 add_var (List.map snd pdecls) parms


(** Traduz uma definicao de funcao *)
let translate_fundef (_, name, parms, stmts, retexp) =
  let parms_type = Array.make (List.length parms) int_type in
  let ft = function_type int_type parms_type in
  let f = match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some _ -> failwith ("Funcao definida mais de uma vez: " ^ name) in
  let stbl = Hashtbl.create 10 in
  let bb = append_block llcontext "entry" f in
    position_at_end bb builder;
    ignore (add_parms f stbl parms (Array.to_list (params f)));
    ignore (List.map (translate_stmt stbl) stmts);
    let retv = translate_expr stbl retexp in
    let _ = build_ret retv builder in
      Llvm_analysis.assert_valid_function f;
      f
	  

let translate_program p = 
  List.map translate_fundef p
