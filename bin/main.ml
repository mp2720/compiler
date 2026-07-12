let () =
  let lexbuf = Lexing.from_channel stdin in
  Compiler.Parser.funcbody Compiler.Lexer.token lexbuf
  |> Compiler.Ast.dump_funcbody |> print_endline
