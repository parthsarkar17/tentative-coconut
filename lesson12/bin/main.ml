let get_trace (interpreter_binary_path : string) (program_path : string)
    (program_args : string) (print_trace : bool) : Bril.Instr.t list =
  (* construct command to get trace from interpreter*)
  let command =
    "/Users/parthsarkar/.local/bin/bril2json < " ^ program_path ^ " | "
    ^ interpreter_binary_path ^ " -t " ^ program_args
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

module StringMap = Map.Make (String)

let bruh x = x

(** Filters a trace to get it ready for insertion back into the Bril program. In
    particular, A) Get rid of Jump instructions, B) Change Branch instructions
    to Guard instructions (If the trace takes the true branch, then the Guard is
    as simple as copying the boolean used in the Branch. If it takes the false
    branch, we need to create and use a variable representing the not() of the
    boolean used in the branch). We leave everything else as-is. *)
let filter_trace (guard_label : string) (label_map : Bril.Instr.t StringMap.t)
    (trace : Bril.Instr.t list) : Bril.Instr.t list =
  let rec filter_trace_aux (acc : Bril.Instr.t list) :
      Bril.Instr.t list -> Bril.Instr.t list = function
    | [] -> acc
    (* need two consecutive instructions in case the first instruction is a branch; 
    if this is the case, we'd need to know which of the true/false branch is taken. *)
    | fst :: snd :: rest -> (
        match fst with
        | Bril.Instr.Jmp _ | Bril.Instr.Label _ | Bril.Instr.Ret _ ->
            filter_trace_aux acc (snd :: rest)
        | Bril.Instr.Br (guard, label1, _) ->
            if snd = StringMap.find label1 label_map then
              filter_trace_aux
                (Bril.Instr.Guard (guard, guard_label) :: acc)
                (snd :: rest)
            else
              let not_guard_var_name = "generated_not_" ^ guard in
              let not_guard =
                Bril.Instr.Unary
                  ( (not_guard_var_name, Bril.Bril_type.BoolType),
                    Bril.Op.Unary.Not,
                    guard )
              in
              filter_trace_aux
                (Bril.Instr.Guard (not_guard_var_name, guard_label)
                :: not_guard :: acc)
                (snd :: rest)
        | _ -> filter_trace_aux (fst :: acc) (snd :: rest))
    (* the case in which there's only one instruction left in the trace *)
    | trace_head :: [] -> (
        match trace_head with
        | Bril.Instr.Jmp _ | Bril.Instr.Label _ | Bril.Instr.Ret _ ->
            filter_trace_aux acc []
        | Bril.Instr.Br (guard, _, _) ->
            filter_trace_aux (Bril.Instr.Guard (guard, guard_label) :: acc) []
        | _ -> filter_trace_aux (trace_head :: acc) [])
  in
  trace |> filter_trace_aux [] |> List.rev

(** Take the filtered trace and inject it into the beginning of the given
    function. We need to pad the trace with Speculate and Commit, and also a
    Jump to a part of the code if the trace code ends up being commited. *)
let insert_into (program_path : string) (program_args : string)
    (label_after_commit : string) (label_map : Bril.Instr.t StringMap.t)
    (func : Bril.Func.t) : Bril.Func.t =
  let trace =
    get_trace "deno run ../../bril/brili.ts " program_path program_args false
    |> filter_trace "__b0" label_map
  in
  let existing_instrs = Bril.Func.instrs func in
  let new_instrs =
    Bril.Instr.Label "trace" :: Bril.Instr.Speculate
    :: (trace
       @ [ Bril.Instr.Commit; Bril.Instr.Jmp label_after_commit ]
       @ existing_instrs
       @ [ Bril.Instr.Label label_after_commit; Bril.Instr.Nop ])
  in
  Bril.Func.set_instrs func new_instrs

(** Provides a map from labels to the first instruction after the label. This is
    used in order to determine which of the true/false branches are taken in a
    trace. *)
let build_map (func : Bril.Func.t) (init : Bril.Instr.t StringMap.t) :
    Bril.Instr.t StringMap.t =
  let rec build_map_aux (instr_list : Bril.Instr.t list)
      (acc : Bril.Instr.t StringMap.t) : Bril.Instr.t StringMap.t =
    match instr_list with
    | [] | [ _ ] -> acc
    | fst :: snd :: rest -> (
        match fst with
        | Bril.Instr.Label label ->
            build_map_aux (snd :: rest) (StringMap.add label snd acc)
        | _ -> build_map_aux (snd :: rest) acc)
  in
  build_map_aux (Bril.Func.instrs func) init

(** Entry point *)
let run (program_path : string) (program_default_args : string) : unit =
  (* get program from stdin *)
  let program : Bril.t =
    "/Users/parthsarkar/.local/bin/bril2json < " ^ program_path
    |> Unix.open_process_in |> In_channel.input_all |> Yojson.Basic.from_string
    |> Bril.from_json
  in
  (* find main func in Bril program *)
  let main : Bril.Func.t =
    List.find (fun (func : Bril.Func.t) -> func.name = "main") program
  in
  (* build a map from labels to the first instr. after the label *)
  let label2first_instr =
    List.fold_left
      (fun acc bril_func -> build_map bril_func acc)
      StringMap.empty program
  in
  (* build a trace, filter & modify the trace, pad with speculate/commit, insert
  dummy label that the commit can jump to after trace has successfully finished. *)
  let main_with_trace : Bril.Func.t =
    insert_into program_path program_default_args "nop.label" label2first_instr
      main
  in
  (* insert this main function back into the list of functions (really, this
  code only works when there are no calls to other functions, but if I had more 
  time this week, I would have tried the interprocedural tracing version. It
  just adds a bit more complexity w.r.t. dealing with returning values,
  storing arguments into variables to replicate functions calls, etc. )*)
  let program_with_trace =
    main_with_trace
    :: List.filter (fun (func : Bril.Func.t) -> func.name <> "main") program
  in
  program_with_trace |> Bril.to_json |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout

(* let () = run "../../bril/benchmarks/core/loopfact.bril" "8" *)

let () = run "../../bril/benchmarks/core/collatz.bril" "7"
