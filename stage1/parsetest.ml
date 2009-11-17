(* 
 *
 * Compilador MiniC em OCaml
 * 
 * Teste do Analisador Sintatico para MiniC
 * 
 * Andrei de A. Formiga, 2009-10-28
 * 
 *)


let main fname = 
  let infile = open_in fname in
  let lexbuf = Lexing.from_channel infile in
  let _ = Parsing.set_trace true in
  let _ = Parser.program Lexer.next_token lexbuf in
  print_endline "Analise completa"
  

let _ = 
  if Array.length Sys.argv < 2 then
    print_endline "Especifique o arquivo de entrada"
  else
    main Sys.argv.(1)

