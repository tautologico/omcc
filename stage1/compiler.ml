(* 
 *
 * Compilador MiniC em OCaml
 * 
 * Modulo principal do compilador. 
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 *)

open Llvm

let main ifname ofname = 
  let infile = open_in ifname in
  let lexbuf = Lexing.from_channel infile in
  let p = Parser.program Lexer.next_token lexbuf in
  let _ = Llvm_codegen.translate_program p in
    if not (Llvm_bitwriter.write_bitcode_file Llvm_codegen.the_module ofname) then 
      exit 1
    else
    (
      dispose_module Llvm_codegen.the_module;
      close_in infile
    )

let print_usage pname = 
  print_endline ("Uso: " ^ pname ^ " <entrada> <saida>")

let _ = 
  if Array.length Sys.argv < 3 then
    print_usage Sys.argv.(0)
  else
    main Sys.argv.(1) Sys.argv.(2)
