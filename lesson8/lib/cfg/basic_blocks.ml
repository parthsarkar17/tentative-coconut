type t = (int * Bril.Instr.t) list Utils.IntValuedMap.t
(** Represents the basic blocks within a Bril function. *)

(** Convert a canonical block representation into just a list of instruction
    lists. *)
let just_blocks (blocks : t) : Bril.Instr.t list list =
  blocks |> Utils.IntValuedMap.to_list
  |> List.map (fun pair -> pair |> snd |> List.map snd)

(** [insert_into_block instr block map] is a [BasicBlock.t] where the Bril
    instruction is inserted into the bacbinding for [block] in [map] *)
let insert_into_block (instr : Bril.Instr.t) (block : int) : t -> t =
  Utils.IntValuedMap.update block (function
    | None -> Some [ (1, instr) ]
    | Some lst ->
        Some
          (match List.rev lst with
          | [] -> [ (1, instr) ]
          | (prev_final_instr_id, prev_final_instr) :: t ->
              List.rev
                ((prev_final_instr_id + 1, prev_final_instr)
                :: (prev_final_instr_id, instr)
                :: t)))

(** [remove_from_block (block_id, instr_id) map] is a [BasicBlock.t] where the
    instruction indexed by [(block_id, instr_id)] is removed from its block. *)
let remove_from_block ((block_id, instr_id) : int * int) : t -> t =
  Utils.IntValuedMap.update block_id (function
    | None -> None
    | Some instrs ->
        Some (List.filter (fun (instr_id', _) -> instr_id <> instr_id') instrs))

let blocks_where_used (instr : Bril.Instr.t) (map : t) : Utils.IntSet.t =
  match Bril.Instr.dest instr with
  | None -> Utils.IntSet.empty
  | Some (dest_var, _) ->
      Utils.IntValuedMap.fold
        (fun block_id indexed_instrs blocks_where_used ->
          if
            List.exists
              (fun (_, instr) ->
                instr |> Bril.Instr.args
                |> List.exists (fun arg -> dest_var = arg))
              indexed_instrs
          then Utils.IntSet.add block_id blocks_where_used
          else blocks_where_used)
        map Utils.IntSet.empty

(** [form_blocks f] is a map from indices of basic blocks to the list of
    instructions forming the block, formed from the function [f]. *)
let form_blocks (func : Bril.Func.t) : t =
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
           Utils.IntValuedMap.add i
             (List.mapi
                (fun instr_index instr -> (instr_index + 1, instr))
                block)
             acc ))
       (1, Utils.IntValuedMap.empty)
  |> snd
