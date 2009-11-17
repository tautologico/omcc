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
let itoa_buffer = 
  define_global "itoa_buffer" 
    (const_stringz llcontext (String.make 20 '\000')) the_module


(** Referencia para funcoes externas *)
let puts = 
  declare_function "puts" 
    (function_type int_type [| str_type |]) the_module

let itoa = 
  declare_function "itoa"
    (function_type str_type [| int_type; str_type; int_type |]) the_module


(** Obtem uma funcao ja compilada para bitcode *)
let get_bc_function fname = match lookup_function fname the_module with
    Some f -> f
  | None -> failwith "Funcao nao encontrada"


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
	  let _ = build_call itoa [| v; buffer_ptr; const_int int_type 10 |] "" builder in
	    ignore (build_call puts [| buffer_ptr |] "" builder)
	else
	  failwith "printint deve imprimir inteiros"
  | Attrib (v, e) -> failwith "atribuicoes nao implementadas ainda (lol)"
  | Block sl -> ignore (List.map (translate_stmt symtbl) sl)


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


(** Traduz uma definicao de funcao *)
let translate_fundef (_, name, parms, stmts, retexp) =
  let parms_type = Array.make (List.length parms) int_type in
  let ft = function_type int_type parms_type in
  let f = match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some _ -> failwith ("Funcao definida mais de uma vez: " ^ name) in
  let stbl = Hashtbl.create 10 in
  let () = add_parms stbl parms (Array.to_list (params f)) in
  let bb = append_block llcontext "entry" f in
    position_at_end bb builder;
    ignore (List.map (translate_stmt stbl) stmts);
    let retv = translate_expr stbl retexp in
    let _ = build_ret retv builder in
      Llvm_analysis.assert_valid_function f;
      f

(* let convert_str e = match e with *)
(*     String s -> const_stringz llcontext s  *)
(*   | _ -> failwith "Expressao nao string" *)

(* let define_string_constants (_, name, parms, stmts, retexp) = *)
(*   let rec collect stmt = match stmt with *)
(*       PrintS e ->        *)
(* 	let c = convert_str e in *)
	  


let translate_program p = 
  List.map translate_fundef p
