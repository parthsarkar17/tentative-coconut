(** This file provides a function to do the small analysis requested in Task 1.
    Specifically, this function analyzes the number of variables instantiated
    using the "const" keyword. *)
let analyze_num_constants () =
  In_channel.input_all In_channel.stdin
  |> Yojson.Basic.from_string |> Bril.from_json
  |> List.fold_left
       (fun count func ->
         count
         + (func |> Bril.Func.instrs
           |> List.fold_left
                (fun sub_count inst ->
                  match inst with
                  | Bril.Instr.Const _ -> sub_count + 1
                  | _ -> sub_count)
                0))
       0
