type t = (int * Bril.Instr.t) list Utils.IntValuedMap.t
(** Represents the basic blocks within a Bril function. *)

(** Convert a canonical block representation into just a list of instruction
    lists. *)
let just_blocks (blocks : t) : Bril.Instr.t list list =
  blocks |> Utils.IntValuedMap.to_list
  |> List.map (fun pair -> pair |> snd |> List.map snd)

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
