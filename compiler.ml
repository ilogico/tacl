let () =
  let file =
    if Array.length Sys.argv > 1
    then open_in (Sys.argv.(1))
    else stdin
  in
  try
    let p = Grammar.program Lexer.token (Lexing.from_channel file) in
    Tacl.compile_program p
  with
    Grammar.Error -> prerr_endline "Syntax error, sorry"
