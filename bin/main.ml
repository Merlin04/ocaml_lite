let () =
  if Array.length Sys.argv <> 2
  then failwith "Expected exactly one command line argument"
  else
    let ch = In_channel.open_text Sys.argv.(1) in
    let text = In_channel.input_all ch in
    let () = In_channel.close ch in
    let ast = Ocaml_lite.Parser.parse text in
(*    let _ = Ocaml_lite.Typechecker.typecheck ast in *)
    let _ = Ocaml_lite.Interpreter.interpret_prog ast in
    ()
