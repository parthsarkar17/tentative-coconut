let get_trace (interpreter_binary_path : string) (program_path : string)
    (program_args : string) (print_trace : bool) : Bril.Instr.t list =
  (* construct command to get trace from interpreter*)
  let command =
    "/Users/parthsarkar/.local/bin/bril2json < " ^ program_path ^ " | "
    ^ interpreter_binary_path ^ " " ^ program_args
  in
  match
    (* interpreter is modified to send list of instructions in json format *)
    command |> Unix.open_process_in |> In_channel.input_lines
    |> String.concat "" |> Yojson.Basic.from_string
    |> fun x ->
    if print_trace then Yojson.Basic.pretty_to_string x |> print_endline else ();
    x
  with
  (* translate this list of json-formatted instrs to a list of bril instrs *)
  | `List lst -> List.map Bril.Instr.of_json lst
  | _ -> failwith "Reference interpreter sends wrong format"

let () =
  let _ =
    get_trace "deno run ../../bril/brili.ts"
      "../../bril/benchmarks/core/loopfact.bril" "8" true
  in
  ()
