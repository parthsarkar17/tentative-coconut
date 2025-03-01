type t = int list Utils.IntValuedMap.t * int list Utils.IntValuedMap.t

(** Returns the predecessors of each block in the control flow graph. *)
let preds : t -> int list Utils.IntValuedMap.t = fst

(** Returns the successors of each block in the control flow graph. *)
let succs : t -> int list Utils.IntValuedMap.t = snd

(** [construct_cfg f] is a pair of maps. The first maps every basic block in the
    function [f] to the indices of its predecessors, and the second maps every
    block to the indices of its successors. *)
let construct_cfg (func : Bril.Func.t) : t =
  (* map index of the basic block to the block's final instruction and the block's instructions *)
  let enumerated_blocks : (int * Bril.Instr.t * Bril.Instr.t list) list =
    func |> Basic_blocks.form_blocks |> Utils.IntValuedMap.to_list
    |> List.map (fun (block_idx, enumerated_instrs) ->
           let instrs = List.map snd enumerated_instrs in
           (block_idx, instrs |> List.rev |> List.hd, instrs))
  in
  (* map the label of block to its index, and get the first index 
        that doesn't belong to any basic block *)
  let label_to_id_map, max_id =
    List.fold_left
      (fun (label_to_id, max_id) (id, _, block) ->
        ( (match List.hd block with
          | Bril.Instr.Label lbl -> Utils.StringValuedMap.add lbl id label_to_id
          | _ -> label_to_id),
          max_id + 1 ))
      (Utils.StringValuedMap.empty, 1)
      enumerated_blocks
  in
  (* produce a new map with [addition] appended to the list bound to [key] in [map] *)
  let update_map_binding_list key addition map =
    Utils.IntValuedMap.update key
      (function
        | None -> Some [ addition ]
        | Some existing_preds -> Some (addition :: existing_preds))
      map
  in
  (* for every block, append each successor to this block's set of successors;
    also, append this block to each successor's set of predecessors *)
  let preds, succs =
    List.fold_left
      (fun ((preds, succs) :
             int list Utils.IntValuedMap.t * int list Utils.IntValuedMap.t)
           ((id, block_last_inst, _) : int * Bril.Instr.t * _) ->
        match block_last_inst with
        | Bril.Instr.Jmp target ->
            let succ = Utils.StringValuedMap.find target label_to_id_map in
            ( update_map_binding_list succ id preds,
              update_map_binding_list id succ succs )
        | Bril.Instr.Br (_, true_branch, false_branch) ->
            [
              Utils.StringValuedMap.find true_branch label_to_id_map;
              Utils.StringValuedMap.find false_branch label_to_id_map;
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
      (Utils.IntValuedMap.empty, Utils.IntValuedMap.empty)
      enumerated_blocks
  in
  (Utils.IntValuedMap.add 1 [ 0 ] preds, Utils.IntValuedMap.add 0 [ 1 ] succs)
