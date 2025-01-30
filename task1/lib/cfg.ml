module CFG = Map.Make (Int)
module LabelsToId = Map.Make (String)
module IdToLabels = Map.Make (Int)

(** [form_blocks body] is a list of basic blocks produced from a list of
    instructions [body] that represents a Bril program (within a function, not
    across functions). *)
let form_blocks (body : Bril.Instr.t list) : Bril.Instr.t list list =
  let final_block, other_blocks =
    List.fold_left
      (fun (curr_block, other_blocks) inst ->
        match inst with
        (* if label, append current basic block to others and make header of new basic block the label *)
        | Bril.Instr.Label _ -> ([ inst ], List.rev curr_block :: other_blocks)
        (* if non-call control insn, then append to current block and append to other blocks *)
        | Bril.Instr.Ret _ | Bril.Instr.Br _ | Bril.Instr.Jmp _ ->
            ([], List.rev (inst :: curr_block) :: other_blocks)
        (* if normal insn, then simply append to current basic block*)
        | _ -> (inst :: curr_block, other_blocks))
      ([], []) body
  in
  (* need to form the remaining instructions that don't have a terminator 
into another block; also, get rid of any empty blocks. *)
  List.rev final_block :: other_blocks
  |> List.filter (fun lst -> lst |> List.is_empty |> not)
  |> List.rev

(** [label_to_id blocks] gives two maps: one map takes a label (at the head of
    the basic block) to the integer Id that we internally use to represent that
    basic block, and the other map does the opposite. Each element of the list
    [blocks] is a 3-tuple, whose first element is that block's Id and whose
    third element is the block itself. *)
let label_to_id (blocks : (int * Bril.Instr.t * Bril.Instr.t list) list) :
    int LabelsToId.t * string IdToLabels.t =
  List.fold_left
    (fun (label_to_id, id_to_labels) (id, _, block) ->
      match List.hd block with
      | Bril.Instr.Label lbl ->
          (LabelsToId.add lbl id label_to_id, IdToLabels.add id lbl id_to_labels)
      | _ -> (label_to_id, id_to_labels))
    (LabelsToId.empty, IdToLabels.empty)
    blocks

(** From a list of instructions, construct the basic blocks of the program and
    form them into a control-flow graph. The CFG data structure is simply a map
    from an (integer) Id of a basic block to the Id of any basic blocks that are
    immediate successors to the current one. *)
let construct_cfg (body : Bril.Instr.t list) : (string * string list) list =
  let enumerated_blocks =
    body |> form_blocks
    (* collect each basic block with its index and final instruction*)
    |> List.mapi (fun i block -> (i, block |> List.rev |> List.hd, block))
  in
  (* Maps from label to id and vice versa*)
  let label_to_id_map, id_to_label_map = label_to_id enumerated_blocks in
  (* The first integer id that should not be a successor (b/c it doesn't exist!) *)
  let max_id = List.length enumerated_blocks in
  (* For every block, take the final instruction and push its successors to the CFG map *)
  List.fold_left
    (fun map (id, block_last_inst, _) ->
      match block_last_inst with
      | Bril.Instr.Jmp target ->
          CFG.add id [ LabelsToId.find target label_to_id_map ] map
      | Bril.Instr.Br (_, true_target, false_target) ->
          CFG.add id
            [
              LabelsToId.find true_target label_to_id_map;
              LabelsToId.find false_target label_to_id_map;
            ]
            map
      (* for arbitrary instructions, point to next block unless block's index is invalid *)
      | _ -> CFG.add id (if id < max_id - 1 then [ id + 1 ] else []) map)
    CFG.empty enumerated_blocks
  (* Finally, convert from integer id's to readable labels again (if they exist in the map; 
        if not, just use the stringified integer labels) *)
  |> CFG.to_list
  |>
  let id_to_string =
   fun id ->
    match IdToLabels.find_opt id id_to_label_map with
    | None -> string_of_int id
    | Some label -> label
  in
  List.map (fun (id, succs) ->
      (id_to_string id, List.map (fun succ -> id_to_string succ) succs))
