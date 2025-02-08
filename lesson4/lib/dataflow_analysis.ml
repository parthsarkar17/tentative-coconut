module type DF_ANALYSIS_TEMPLATE = sig
  type t
  (** An abstract type to represent a member of the lattice (e.g. could be a set
      in the powerset lattice.) *)

  val empty : t
  (** The top value in a lattice. *)

  val init : Bril.Func.t -> t
  (** [init func] is a lattice member that forms as a result of the arguments of
      function [func]. *)

  val equal : t -> t -> bool

  val transfer : (int * int * Bril.Instr.t) list -> t -> t
  (** [transfer block mem] is a new member of the lattice that's a result of the
      contents of the basic block [block] applied to the input lattice member
      [mem]. Because we might need to identify specific instructions, a basic
      block here is more than just a list of instructions; the first index of
      each member of [block] refers to the basic block, and the second index
      refers to the instruction within the block. *)

  val merge : t -> t -> t
  (** The abstract definition for the meet operator over two members of the
      lattice. For example, could be the lowest upper bound of two sets in a
      powerset lattice (i.e. set union). *)
end

(** A data type for the elements of each lattice member in the powerset lattice
    where each set represents the reaching definitions for a basic block. *)
module Definition = struct
  type t = int * int * string

  let compare (b1, i1, d1) (b2, i2, d2) =
    match Int.compare b1 b2 with
    | 0 -> ( match Int.compare i1 i2 with 0 -> String.compare d1 d2 | m -> m)
    | n -> n
end

(** Fill out the data flow analysis form for reaching analysis. *)
module ReachingAnalysis : DF_ANALYSIS_TEMPLATE = struct
  module DefinitionSet = Set.Make (Definition)

  type t = DefinitionSet.t

  let empty = DefinitionSet.empty

  let init (func : Bril.Func.t) : t =
    func.args
    |> List.fold_left
         (fun (acc, i) (dest_name, _) ->
           (DefinitionSet.add (0, i, dest_name) acc, i + 1))
         (DefinitionSet.empty, 0)
    |> fst

  let equal = DefinitionSet.equal

  let transfer (block : (int * int * Bril.Instr.t) list) (in_b : t) : t =
    block
    (* get the definitions defined in this block *)
    |> List.fold_left
         (fun acc (b, i, instr) ->
           match Bril.Instr.dest instr with
           | None -> acc
           | Some (dest_name, _) -> DefinitionSet.add (b, i, dest_name) acc)
         DefinitionSet.empty
    (* unite them with the definitions remaining after removing those killed in this block *)
    |> DefinitionSet.union
         (List.fold_left
            (fun acc (_, _, instr) ->
              match Bril.Instr.dest instr with
              | None -> acc
              | Some (instr_dest_name, _) ->
                  DefinitionSet.filter
                    (fun (_, _, elt_dest_name) ->
                      elt_dest_name != instr_dest_name)
                    acc)
            in_b block)

  let merge = DefinitionSet.union
end

module DataFlowAnalysis (Lattice : DF_ANALYSIS_TEMPLATE) = struct
  (* define map modules for storing cfg representation, etc. *)
  module IntValuedMap = Map.Make (Int)
  module StringValuedMap = Map.Make (String)

  (** [form_blocks f] is a map from indices of basic blocks to the list of
      instructions forming the block, formed from the function [f]. *)
  let form_blocks (func : Bril.Func.t) :
      (int * Bril.Instr.t) list IntValuedMap.t =
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
         (fun (i, acc) block ->
           ( i + 1,
             (* map this block's index to an enumerated list of its instructions *)
             IntValuedMap.add i
               (List.mapi (fun instr_index instr -> (instr_index, instr)) block)
               acc ))
         (1, IntValuedMap.empty)
    |> snd

  (** [construct_cfg f] is a pair of maps. The first maps every basic block in
      the function [f] to the indices of its predecessors, and the second maps
      every block to the indices of its successors. *)
  let construct_cfg (func : Bril.Func.t) :
      int list IntValuedMap.t * int list IntValuedMap.t =
    (* map index of the basic block to the block's final instruction and the block's instructions *)
    let enumerated_blocks : (int * Bril.Instr.t * Bril.Instr.t list) list =
      func |> form_blocks |> IntValuedMap.to_list
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
            | Bril.Instr.Label lbl -> StringValuedMap.add lbl id label_to_id
            | _ -> label_to_id),
            max_id + 1 ))
        (StringValuedMap.empty, 1) enumerated_blocks
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

  let worklist (func : Bril.Func.t) :
      Lattice.t IntValuedMap.t * Lattice.t IntValuedMap.t =
    (* map from the index of each basic block to a list of enumerated instructions *)
    let basic_blocks = form_blocks func in
    (* maps from the index of each basic block to a list of indices of its predecessors and successors, respectively *)
    let preds, succs = construct_cfg func in

    let rec worklist_aux in_map out_map = function
      | [] -> (in_map, out_map)
      | (block_idx, block) :: worklist ->
          let in_block =
            List.fold_left
              (fun acc pred ->
                Lattice.merge acc
                  (match IntValuedMap.find_opt pred out_map with
                  | None -> Lattice.empty
                  | Some set -> set))
              Lattice.empty
              (IntValuedMap.find block_idx preds)
          in
          let out_block =
            Lattice.transfer
              (List.map
                 (fun (instr_idx, instr) -> (block_idx, instr_idx, instr))
                 block)
              in_block
          in
          (* if there's a change in this block's output lattice member, 
              then add its successors to the worklist*)
          (if
             match IntValuedMap.find_opt block_idx out_map with
             | None -> true
             | Some existing_out_block ->
                 existing_out_block |> Lattice.equal out_block |> not
           then
             List.fold_left
               (fun acc succ_idx ->
                 (succ_idx, IntValuedMap.find succ_idx basic_blocks) :: acc)
               []
               (IntValuedMap.find block_idx succs)
             @ worklist
           else worklist)
          |> worklist_aux
               (* replace the lattice member at the input of this block *)
               (IntValuedMap.update block_idx
                  (function _ -> Some in_block)
                  in_map)
               (* replace the lattice member at the output of this block *)
               (IntValuedMap.update block_idx
                  (function _ -> Some out_block)
                  out_map)
    in
    worklist_aux
      (IntValuedMap.singleton 0 (Lattice.init func))
      IntValuedMap.empty
      (IntValuedMap.to_list basic_blocks)
end
