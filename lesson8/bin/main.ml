(** [insert_dummy func] is a transformed Bril function, where a new entry basic
    block is inserted to represent the arguments to function [func]. If [func]
    has no arguments, then the exisitng entry block will be given a label if it
    doesn't have one already. *)
let insert_dummy (func : Bril.Func.t) : Bril.Func.t =
  match (func.args, func |> Bril.Func.instrs |> List.hd) with
  (* if func has no arguments and first block has a label, no changes *)
  | [], Bril.Instr.Label _ -> func
  (* if func has no arguments and first block isn't a label, insert label *)
  | [], _ ->
      Bril.Func.set_instrs func
        (Bril.Instr.Label "entry" :: Bril.Func.instrs func)
  (* if func has an argument x E args, then create dummy block with the 
      instructions x : _ = id x for all x. also, insert label for dummy block *)
  | arg_list, first_instr ->
      let function_instrs = Bril.Func.instrs func in
      let has_entry_label =
        match first_instr with Bril.Instr.Label _ -> true | _ -> false
      in
      let dummy_block_contents =
        (arg_list
        |> List.map (fun (arg : Bril.Dest.t) ->
               Bril.Instr.Unary (arg, Bril.Op.Unary.Id, fst arg)))
        @ (if has_entry_label then [] else [ Bril.Instr.Label "entry" ])
        @ function_instrs
      in
      Bril.Func.set_instrs func
        (Bril.Instr.Label "entry1" :: dummy_block_contents)

let _iter_func (f : Bril.Func.t -> unit) : unit =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> List.iter f

(* let () =
  iter_func (fun f -> f |> insert_dummy |> Lesson8.Licm.print_insert_preheaders) *)

let () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map (fun func -> func |> insert_dummy |> Lesson8.Licm.transform)
  |> Bril.to_json |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout
