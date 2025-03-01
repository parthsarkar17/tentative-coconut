open Utils

module Pair = struct
  type t = string * int

  let compare (v1, b1) (v2, b2) =
    match (String.compare v1 v2, Int.compare b1 b2) with 0, n -> n | n, _ -> n
end

module PairValuedMap = Map.Make (Pair)

(** [phi_nodes_of f] is a map taking a variable-block pair to a unique variable
    used to represent the variable at the phi node at the beginning of the
    block; naive implementation of SSA. *)
let phi_nodes_of (func : Bril.Func.t) : string PairValuedMap.t =
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
                PairValuedMap.add (var, block_id)
                  (var ^ "." ^ string_of_int block_id ^ ".1"))
              func_vars
         |> fun x -> (x, block_id + 1))
       (PairValuedMap.empty, 1)
  |> fst

(** [transform_block id b p2u] is a tuple. The first element is a basic block
    constructed from [b], transformed to use the variable rewrites in [p2u]
    (which is a total map from original variables to new variables) and that
    assigns new variables for each local variable overwrite. The second elmement
    is a total map from each original variable to the newest local variable
    created to represents that variable. Will be used to insert `set` nodes. *)
let transform_block (block_id : int) (pair2unique : string PairValuedMap.t)
    (block : (int * Bril.Instr.t) list) :
    Bril.Instr.t list * string StringValuedMap.t =
  block
  |> List.fold_left
       (fun ((rev_transformed_block, ogvar2newestvar) :
              Bril.Instr.t list * string StringValuedMap.t)
            ((instr_id, instr) : int * Bril.Instr.t) ->
         let instr_args_transformed : Bril.Instr.t =
           match Bril.Instr.args instr with
           | [] -> instr
           | lst -> (
               try
                 Bril.Instr.set_args
                   (List.map
                      (fun arg -> StringValuedMap.find arg ogvar2newestvar)
                      lst)
                   instr
               with _ -> instr)
         in
         match Bril.Instr.dest instr_args_transformed with
         | None ->
             (instr_args_transformed :: rev_transformed_block, ogvar2newestvar)
         | Some (dest_var, dest_type) ->
             let new_dest_var =
               ogvar2newestvar
               |> StringValuedMap.find dest_var
               |> String.to_seq |> List.of_seq |> List.rev |> List.tl
               |> List.rev
               |> List.map (String.make 1)
               |> String.concat ""
               |> ( ^ ) (string_of_int instr_id)
             in
             ( Bril.Instr.set_dest
                 (Some (new_dest_var, dest_type))
                 instr_args_transformed
               :: rev_transformed_block,
               StringValuedMap.update dest_var
                 (function _ -> Some new_dest_var)
                 ogvar2newestvar ))
       ( [],
         (* construct total mapping from og variable to newest local representation of
              variable. *)
         PairValuedMap.fold
           (fun ((og_var, block_id') : Pair.t) (new_var : string)
                (acc : string StringValuedMap.t) ->
             if block_id = block_id' then StringValuedMap.add og_var new_var acc
             else acc)
           pair2unique StringValuedMap.empty )
  |> fun (rev_transformed_block, y) -> (List.rev rev_transformed_block, y)
