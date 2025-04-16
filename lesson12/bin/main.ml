let get_trace (interpreter_binary_path : string) (program_path : string)
    (program_args : string) : unit =
  (* construct command to run modified reference interpreter *)
  let json_transformer = "/Users/parthsarkar/.local/bin/bril2json < " in
  json_transformer ^ program_path ^ " | " ^ interpreter_binary_path ^ " "
  ^ program_args
  (* expect the reference interpreter to only log json, string format of its trace *)
  |> Unix.open_process_in
  |> In_channel.input_line |> Option.get
  (* for now, just print out this representation *)
  |> print_endline

let () =
  get_trace "/Users/parthsarkar/.deno/bin/brili"
    "../../bril/benchmarks/core/hanoi.bril" "3"
