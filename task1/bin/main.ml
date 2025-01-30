(* prints out the basic blocks and CFG of a bril program in JSON form *)
let () =
  let bril_rep =
    In_channel.input_all In_channel.stdin
    |> Yojson.Basic.from_string |> Bril.from_json
  in
  print_newline ();
  bril_rep
  |> List.iter (fun func ->
         func |> Bril.Func.instrs |> Task1.Cfg.form_blocks
         |> List.iter (fun inst_list ->
                List.iter
                  (fun inst ->
                    inst |> Bril.Instr.to_string |> ( ^ ) ";  " |> print_string)
                  inst_list;
                print_newline ()));
  print_newline ();
  bril_rep
  |> List.iter (fun func ->
         func |> Bril.Func.instrs |> Task1.Cfg.construct_cfg
         |> List.iter (fun (label, succs) ->
                print_endline
                  ("label: " ^ label ^ ", succs: "
                  ^ List.fold_left (fun acc s -> acc ^ "  " ^ s) "" succs)));
  print_newline ()
