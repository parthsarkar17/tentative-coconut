open Utils

module Pair = struct
  type t = string * int

  let compare (v1, b1) (v2, b2) =
    match (String.compare v1 v2, Int.compare b1 b2) with 0, n -> n | n, _ -> n

  let to_string ((v, b) : t) = "(" ^ v ^ ", " ^ string_of_int b ^ ")"
end

module PairValuedMap = Map.Make (Pair)

(** [phi_nodes_of f] is a map taking a variable-block pair to a unique variable
    used to represent the variable at the phi node at the beginning of the
    block; naive implementation of SSA. *)
let phi_nodes_of (func : Bril.Func.t) : string PairValuedMap.t =
  let func_vars : StringSet.t =
    func |> Bril.Func.instrs (* include every variable ever defined *)
    |> List.fold_left
         (fun (set : StringSet.t) (instr : Bril.Instr.t) ->
           match Bril.Instr.dest instr with
           | None -> set
           | Some (var, _) -> StringSet.add var set)
         StringSet.empty
    |> StringSet.union (* also include arguments to function *)
         (func.args
         |> List.fold_left
              (fun acc (arg, _) -> StringSet.add arg acc)
              StringSet.empty)
  in
  (* map every variable-basic block pair to a unique, local version of the variable *)
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
    assigns new variables for each local variable overwrite. The second element
    is a total map from each original variable to the newest local variable
    created to represents that variable. This will be used to insert `set`
    nodes. *)
let transform_block (block_id : int) (pair2unique : string PairValuedMap.t)
    (block : (int * Bril.Instr.t) list) :
    Bril.Instr.t list * string StringValuedMap.t =
  List.fold_left
    (fun ((rev_transformed_block, ogvar2newestvar) :
           Bril.Instr.t list * string StringValuedMap.t)
         ((instr_id, instr) : int * Bril.Instr.t) ->
      (* transform the arguments of a function wrt most recent unique name *)
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
      (* update the most recent unique name if this instruction writes to something  *)
      match Bril.Instr.dest instr_args_transformed with
      | None ->
          (instr_args_transformed :: rev_transformed_block, ogvar2newestvar)
      | Some (dest_var, dest_type) ->
          let new_dest_var =
            ogvar2newestvar
            |> StringValuedMap.find dest_var
            |> String.to_seq |> List.of_seq |> List.rev |> List.tl |> List.rev
            |> List.map (String.make 1)
            |> String.concat ""
            |> fun x -> x ^ string_of_int instr_id
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
    block
  |> fun (x, y) -> (List.rev x, y)

let into_ssa (func : Bril.Func.t) : Bril.Func.t =
  let pair2unique = phi_nodes_of func in
  let x =
    func |> Basic_blocks.form_blocks |> IntValuedMap.bindings
    (* transform the block to use latest, local unique variable names *)
    |> List.map
         (fun ((block_id, indexed_block) : int * (int * Bril.Instr.t) list) ->
           let transformed_block, latest_vars =
             transform_block block_id pair2unique indexed_block
           in
           (block_id, transformed_block, latest_vars))
    (* add get instructions at the beginning of every basic block *)
    |> List.map
         (fun
           ((block_id, block_instrs, original2latest) :
             int * Bril.Instr.t list * string StringValuedMap.t)
         ->
           []
           |> PairValuedMap.fold
                (fun (_, other_block_id) unique_local acc ->
                  if block_id = other_block_id then unique_local :: acc else acc)
                pair2unique
           |> fun _ -> ())
  in
  func

let test_fn (func : Bril.Func.t) : unit =
  let blocks = Basic_blocks.form_blocks func in
  let pair2unique = phi_nodes_of func in
  print_endline "pair2unique:";
  PairValuedMap.iter
    (fun (p : Pair.t) (new_var : string) ->
      print_endline ("  " ^ Pair.to_string p ^ ": " ^ new_var))
    pair2unique;
  print_endline "";
  IntValuedMap.empty
  |> IntValuedMap.fold
       (fun block_id block new_map ->
         IntValuedMap.add block_id
           (block |> transform_block block_id pair2unique |> fst)
           new_map)
       blocks
  |> IntValuedMap.bindings |> List.map snd |> List.concat
  |> Bril.Func.set_instrs func |> Bril.Func.to_string |> print_endline
