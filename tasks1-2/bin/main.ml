open Task1

(** prints out the basic blocks and CFG of a bril program in JSON form. just
    used to evaluate correctness, not meant for stdout. *)
let _print_bb_cfg () =
  let bril_rep =
    In_channel.input_all In_channel.stdin
    |> Yojson.Basic.from_string |> Bril.from_json
  in
  print_newline ();
  bril_rep (* print basic blocks *)
  |> List.iter (fun func ->
         func |> Bril.Func.instrs |> Task1.Cfg.form_blocks
         |> List.iter (fun inst_list ->
                List.iter
                  (fun inst ->
                    inst |> Bril.Instr.to_string |> ( ^ ) ";  " |> print_string)
                  inst_list;
                print_newline ()));
  print_newline ();
  bril_rep (* print control flow graph *)
  |> List.iter (fun func ->
         func |> Bril.Func.instrs |> Task1.Cfg.construct_cfg
         |> List.iter (fun (label, succs) ->
                print_endline
                  ("label: " ^ label ^ ", succs: "
                  ^ List.fold_left (fun acc s -> acc ^ "  " ^ s) "" succs)));
  print_newline ()

(** displays the output of the small analysis to stdout*)
let _basic_analysis_to_stdout () =
  () |> Task1.Basic_analysis.analyze_num_constants |> Int.to_string
  |> Out_channel.output_string Out_channel.stdout

let tdce prog =
  prog |> Task1.Tdce.elim_global_unused_assigns
  |> Task1.Tdce.elim_locally_killed_assigns

(** run both global and local unused assignment passes *)
let _test_tdce () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> tdce |> Bril.to_json |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout

let _test_lvn () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> Lvn.lvn |> tdce |> Bril.to_json |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout

let _get_results () =
  "./results.csv" |> Csv.load |> Task1.Optim_benefits.produce_results

(* let x = Csv.print_readable *)
let () = _get_results ()
