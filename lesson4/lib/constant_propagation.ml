module Constant = struct
  type t = { var : string; value : Bril.Const.t }

  let to_string c = c.var ^ " = " ^ Bril.Const.to_string c.value

  let compare c1 c2 =
    match String.compare c1.var c2.var with
    | 0 -> Bril.Const.compare c1.value c2.value
    | n -> n
end

module ConstantPropagationLattice = struct
  module ConstantSet = Set.Make (Constant)

  type t = ConstantSet.t

  let empty = ConstantSet.empty
  let init _ = ConstantSet.empty
  let equal = ConstantSet.equal

  (** [transfer block s] are the constants that come into [block], united with
      new constants that are defined in [block] (if there's a variable name
      collision, the new definition is favored), with non-constant variables
      that were once constants being removed. *)
  let transfer block set =
    List.fold_left
      (fun (reduced_set : ConstantSet.t) ((_, _, instr) : _ * _ * Bril.Instr.t)
         ->
        match instr with
        (* remove previous constant variable if there's a name collision;
          replace it with the current constant variable *)
        | Bril.Instr.Const ((dest_name, _), const_value) ->
            ConstantSet.filter (fun elt -> elt.var != dest_name) reduced_set
            |> ConstantSet.add Constant.{ var = dest_name; value = const_value }
        (* get rid of any constants that are overwritten by non-constants in this block *)
        | non_const_instr -> (
            match Bril.Instr.dest non_const_instr with
            | None -> reduced_set
            | Some (dest_name, _) ->
                ConstantSet.filter (fun elt -> elt.var != dest_name) reduced_set
            ))
      set block

  (** [merge s1 s2] are the set of constants that we can guarantee only have one
      value up to this basic block. That is, if sets [s1] and [s2] each contain
      a constant [c], then [c] will not be the evaluated set. *)
  let merge s1 s2 = ConstantSet.union s1 s2
  (* ConstantSet.diff (ConstantSet.union s1 s2) (ConstantSet.inter s1 s2) *)

  let to_string s =
    ConstantSet.fold (fun elt acc -> acc ^ Constant.to_string elt ^ " ") s ""
end

module ConstantPropagation =
  Dataflow_analysis.DataFlowAnalysis (ConstantPropagationLattice)

let global_constant_propagation : Bril.t -> Bril.t =
  List.map (fun func ->
      let in_block_map, _ = ConstantPropagation.worklist_algorithm func in
      func |> ConstantPropagation.form_blocks
      |> ConstantPropagation.IntValuedMap.to_list
      |> List.map (fun (block_idx, enumerated_instrs) ->
             (* set of determined constants at the beginning of this block *)
             let set_of_consts_in =
               ConstantPropagation.IntValuedMap.find block_idx in_block_map
             in
             (* map each variable to the number of incoming constants that share that variable name *)
             let vars2num_consts =
               ConstantPropagationLattice.ConstantSet.fold
                 (fun const acc ->
                   ConstantPropagation.StringValuedMap.update const.var
                     (function
                       | None -> Some 1 | Some existing -> Some (existing + 1))
                     acc)
                 set_of_consts_in ConstantPropagation.StringValuedMap.empty
             in
             enumerated_instrs
             |> List.fold_left
                  (fun acc (_, instr) ->
                    (match instr with
                    | Bril.Instr.Binary (_, bop, arg1, arg2) -> (
                        match
                          ( ConstantPropagation.StringValuedMap.find_opt arg1
                              vars2num_consts,
                            ConstantPropagation.StringValuedMap.find_opt arg2
                              vars2num_consts )
                        with
                        | Some 1, Some 1 -> instr
                        | _ -> instr)
                    | _ -> instr)
                    :: acc)
                  []
             |> List.rev)
      |> List.concat |> Bril.Func.set_instrs func)
