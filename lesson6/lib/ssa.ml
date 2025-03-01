open Utils

module Pair = struct
  type t = string * int

  let compare (v1, b1) (v2, b2) =
    match (String.compare v1 v2, Int.compare b1 b2) with 0, n -> n | n, _ -> n
end

module PairValuedMap = Map.Make (Pair)

(** [unique_identifiers_of_func f] is a map taking a variable-block pair to a
    unique variable used to represent the variable at the phi node at the
    beginning of the block; naive implementation of SSA. *)
let unique_identifiers_of_func (func : Bril.Func.t) : string PairValuedMap.t =
  let func_vars : StringSet.t =
    func |> Bril.Func.instrs
    |> List.fold_left
         (fun (set : StringSet.t) (instr : Bril.Instr.t) ->
           match Bril.Instr.dest instr with
           | None -> set
           | Some (var, _) -> StringSet.add var set)
         StringSet.empty
  in
  func |> Basic_blocks.form_blocks |> Basic_blocks.just_blocks
  |> List.fold_left
       (fun ((pair2unique, block_id) : string PairValuedMap.t * int) _ ->
         pair2unique
         |> StringSet.fold
              (fun (var : string) ->
                PairValuedMap.add (var, block_id) (var ^ string_of_int block_id))
              func_vars
         |> fun x -> (x, block_id + 1))
       (PairValuedMap.empty, 1)
  |> fst
