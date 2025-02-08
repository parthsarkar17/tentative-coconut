module type DF_ANALYSIS_TEMPLATE = sig
  type t

  val empty : t
  val init : Bril.Func.t -> t
  val transfer : (int * Bril.Instr.t) list -> t -> t
  val merge : t -> t -> t
end

module Definition = struct
  type t = int * string

  let compare (a1, b1) (a2, b2) =
    match Int.compare a1 a2 with 0 -> String.compare b1 b2 | n -> n
end

module ReachingAnalysis : DF_ANALYSIS_TEMPLATE = struct
  module DefinitionSet = Set.Make (Definition)

  type t = DefinitionSet.t

  let empty = DefinitionSet.empty

  let init (func : Bril.Func.t) : t =
    func.args
    |> List.fold_left
         (fun (acc, i) (dest_name, _) ->
           (DefinitionSet.add (i, dest_name) acc, i + 1))
         (DefinitionSet.empty, 0)
    |> fst

  let transfer (block : (int * Bril.Instr.t) list) (in_b : t) : t =
    (* get the variables defined in this block*)
    block
    |> List.fold_left
         (fun acc (i, instr) ->
           match Bril.Instr.dest instr with
           | None -> acc
           | Some (dest_name, _) -> DefinitionSet.add (i, dest_name) acc)
         DefinitionSet.empty
    (* unite them with the definitions remaining after removing those killed in this block *)
    |> DefinitionSet.union
         (List.fold_left
            (fun acc (_, instr) ->
              match Bril.Instr.dest instr with
              | None -> acc
              | Some (instr_dest_name, _) ->
                  DefinitionSet.filter
                    (fun (_, elt_dest_name) -> elt_dest_name != instr_dest_name)
                    acc)
            in_b block)

  let merge = DefinitionSet.union
end

module DataFlowAnalysis (Template : DF_ANALYSIS_TEMPLATE) = struct
  module IntValuedMap = Map.Make (Int)
  module StringValuedMap = Map.Make (String)

  (** [form_blocks f] is a map from indices of basic blocks to the list of
      instructions forming the block, formed from the function [f]. *)
  let form_blocks (func : Bril.Func.t) : Bril.Instr.t list IntValuedMap.t =
    let final_block, other_blocks =
      func |> Bril.Func.instrs
      |> List.fold_left
           (fun (curr_block, other_blocks) inst ->
             match inst with
             | Bril.Instr.Label _ ->
                 ([ inst ], List.rev curr_block :: other_blocks)
             | Bril.Instr.Ret _ | Bril.Instr.Br _ | Bril.Instr.Jmp _ ->
                 ([], List.rev (inst :: curr_block) :: other_blocks)
             | _ -> (inst :: curr_block, other_blocks))
           ([], [])
    in
    List.rev final_block :: other_blocks
    |> List.filter (function [] -> false | _ -> true)
    |> List.rev
    |> List.fold_left
         (fun (i, acc) block -> (i + 1, IntValuedMap.add i block acc))
         (0, IntValuedMap.empty)
    |> snd

  (** [construct_cfg f] is a pair of maps. The first maps every basic block in
      the function [f] to the indices of its predecessors, and the second maps
      every block to the indices of its successors. *)
  let construct_cfg (func : Bril.Func.t) :
      int list IntValuedMap.t * int list IntValuedMap.t =
    (* map index of the basic block to the block's final instruction and the block itself*)
    let enumerated_blocks : (int * Bril.Instr.t * Bril.Instr.t list) list =
      func |> form_blocks |> IntValuedMap.to_list
      |> List.map (fun (i, block) -> (i, block |> List.rev |> List.hd, block))
    in
    (* map the label of block to its index and vice versa *)
    let label_to_id_map, max_id =
      List.fold_left
        (fun (label_to_id, max_id) (id, _, block) ->
          ( (match List.hd block with
            | Bril.Instr.Label lbl -> StringValuedMap.add lbl id label_to_id
            | _ -> label_to_id),
            max_id + 1 ))
        (StringValuedMap.empty, 0) enumerated_blocks
    in
    (* produce a new map with [addition] appended to the list bound to [key] in [map] *)
    let update_map_binding_list key addition map =
      IntValuedMap.update key
        (function
          | None -> Some [ addition ]
          | Some existing_preds -> Some (addition :: existing_preds))
        map
    in
    (* for every block, append each successor to this block's set of successors;
    also, append this block to each successor's set of predecessors *)
    List.fold_left
      (fun ((preds, succs) : int list IntValuedMap.t * int list IntValuedMap.t)
           ((id, block_last_inst, _) : int * Bril.Instr.t * _) ->
        match block_last_inst with
        | Bril.Instr.Jmp target ->
            let succ = StringValuedMap.find target label_to_id_map in
            ( update_map_binding_list succ id preds,
              update_map_binding_list id succ succs )
        | Bril.Instr.Br (_, true_branch, false_branch) ->
            [
              StringValuedMap.find true_branch label_to_id_map;
              StringValuedMap.find false_branch label_to_id_map;
            ]
            |> List.fold_left
                 (fun (preds_acc, succs_acc) succ ->
                   ( update_map_binding_list succ id preds_acc,
                     update_map_binding_list id succ succs_acc ))
                 (preds, succs)
        (* for arbitrary instructions, point to next block unless it DNE *)
        | _ ->
            let succ = id + 1 in
            if id < max_id - 1 then
              ( update_map_binding_list succ id preds,
                update_map_binding_list id succ succs )
            else (preds, succs))
      (IntValuedMap.empty, IntValuedMap.empty)
      enumerated_blocks

  let worklist (func : Bril.Func.t) =
    let basic_blocks = form_blocks func in
    let preds, succs = construct_cfg func in

    let rec worklist_aux in_map out_map = function
      | [] -> (in_map, out_map)
      | block :: worklist -> (in_map, out_map)
    in
    ()
end
