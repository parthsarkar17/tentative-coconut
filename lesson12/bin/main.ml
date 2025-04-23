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
  | `List lst ->
      lst |> List.map Bril.Instr.of_json |> fun lst ->
      if print_trace then
        List.iter
          (fun instr -> instr |> Bril.Instr.to_string |> print_endline)
          lst
      else ();
      lst
  | _ -> failwith "Reference interpreter sends wrong format"

let filter_trace (terminating_instr : Bril.Instr.t) (guard_label : string)
    (trace : Bril.Instr.t list) : Bril.Instr.t list =
  let rec filter_trace_aux (acc : Bril.Instr.t list) :
      Bril.Instr.t list -> Bril.Instr.t list = function
    | [] -> acc
    | trace_head :: trace_rest -> (
        match trace_head with
        | Bril.Instr.Jmp _ | Bril.Instr.Label _ ->
            filter_trace_aux acc trace_rest
        | Bril.Instr.Br (guard, _, _) ->
            filter_trace_aux
              (Bril.Instr.Guard (guard, guard_label) :: acc)
              trace_rest
        | _ ->
            if trace_head = terminating_instr then acc
            else filter_trace_aux (trace_head :: acc) trace_rest)
  in
  trace |> filter_trace_aux [] |> List.rev

let insert_into (program_path : string) (program_args : string)
    (func : Bril.Func.t) : Bril.Func.t =
  let trace =
    get_trace "deno run ../../bril/brili.ts" program_path program_args false
    |> filter_trace
         ("{ \"args\": [ \"result\" ], \"dest\": \"v13\", \"op\": \"id\", \
           \"type\": \"int\" }" |> Yojson.Basic.from_string
        |> Bril.Instr.of_json)
         "__b0"
  in
  let existing_instrs = Bril.Func.instrs func in
  let new_instrs =
    Bril.Instr.Label "trace" :: Bril.Instr.Speculate
    :: (trace
       @ [ Bril.Instr.Commit; Bril.Instr.Jmp "for.end.2" ]
       @ existing_instrs)
  in
  Bril.Func.set_instrs func new_instrs

let run (program_path : string) (program_args : string) : unit =
  let program : Bril.t =
    "/Users/parthsarkar/.local/bin/bril2json < " ^ program_path
    |> Unix.open_process_in |> In_channel.input_all |> Yojson.Basic.from_string
    |> Bril.from_json
  in
  let _main : Bril.Func.t =
    List.find (fun (func : Bril.Func.t) -> func.name = "main") program
  in
  let main_with_trace : Bril.Func.t =
    insert_into program_path program_args _main
  in
  let program_with_trace =
    main_with_trace
    :: List.filter (fun (func : Bril.Func.t) -> func.name <> "main") program
  in
  program_with_trace |> Bril.to_json |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout

let () = run "../../bril/benchmarks/core/loopfact.bril" "8"
