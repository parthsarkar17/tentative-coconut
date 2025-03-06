open Utils

module Pair = struct
  type t = string * int

  let compare (v1, b1) (v2, b2) =
    match (String.compare v1 v2, Int.compare b1 b2) with 0, n -> n | n, _ -> n

  let to_string (v, b) = "(" ^ v ^ ", " ^ string_of_int b ^ ")"
end

module PairValuedMap = Map.Make (Pair)
module DestSet = Set.Make (Bril.Dest)

let dests_written_to (func : Bril.Func.t) =
  func |> Bril.Func.instrs (* include every variable ever defined *)
  |> List.fold_left
       (fun (set : DestSet.t) (instr : Bril.Instr.t) ->
         match Bril.Instr.dest instr with
         | None -> set
         | Some dest -> DestSet.add dest set)
       DestSet.empty
  |> DestSet.union (* also include arguments to function *)
       (func.args
       |> List.fold_left (fun acc dest -> DestSet.add dest acc) DestSet.empty)

(** [phi_nodes_of f] is a map taking a variable-block pair to a unique variable
    used to represent the variable at the phi node at the beginning of the
    block; naive implementation of SSA. *)
let phi_nodes_of (func : Bril.Func.t) : Bril.Dest.t PairValuedMap.t =
  let func_dests = dests_written_to func in
  (* map every variable-basic block pair to a unique, local version of the variable *)
  func |> Basic_blocks.form_blocks |> Basic_blocks.just_blocks
  |> List.fold_left
       (fun ((pair2unique, block_id) : Bril.Dest.t PairValuedMap.t * int) _ ->
         pair2unique
         |> DestSet.fold
              (fun ((var, var_type) : Bril.Dest.t) ->
                PairValuedMap.add (var, block_id)
                  (var ^ "." ^ string_of_int block_id ^ ".1", var_type))
              func_dests
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
let transform_block (block_id : int) (pair2unique : Bril.Dest.t PairValuedMap.t)
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
        (fun ((og_var, block_id') : Pair.t) ((new_var, _) : Bril.Dest.t)
             (acc : string StringValuedMap.t) ->
          if block_id = block_id' then StringValuedMap.add og_var new_var acc
          else acc)
        pair2unique StringValuedMap.empty )
    block
  |> fun (x, y) -> (List.rev x, y)

let func_into_ssa (func : Bril.Func.t) :
    Bril.Func.t * ((string * string) * Bril.Bril_type.t) list =
  let (pair2unique, basic_blocks, succs) :
      Bril.Dest.t PairValuedMap.t * Basic_blocks.t * int list IntValuedMap.t =
    ( phi_nodes_of func,
      Basic_blocks.form_blocks func,
      Cfg.(func |> construct_cfg |> succs) )
  in
  let setargs2type : (string * string, Bril.Bril_type.t) Hashtbl.t =
    Hashtbl.create
      ((func |> dests_written_to |> DestSet.cardinal)
      * IntValuedMap.cardinal basic_blocks
      * 2)
  in
  basic_blocks |> IntValuedMap.bindings
  (* transform the block to use latest, local unique variable names *)
  |> List.map (fun (block_id, indexed_block) ->
         let transformed_block, latest_vars =
           transform_block block_id pair2unique indexed_block
         in
         (block_id, transformed_block, latest_vars))
  (* add get instructions at the beginning of every basic block *)
  |> List.map (fun (block_id, block_instrs, original2latest) ->
         []
         |> PairValuedMap.fold
              (fun (_, other_block_id) unique_local acc ->
                if block_id = other_block_id then unique_local :: acc else acc)
              pair2unique
         |> List.map (fun (dest : Bril.Dest.t) -> Bril.Instr.Get dest)
         |> fun gets ->
         ( block_id,
           Basic_blocks.insert_top_of_block gets block_instrs,
           original2latest ))
  (* add get instructions at the beginning of every basic block *)
  |> List.map (fun (block_id, block_instrs, original2latest) ->
         List.map
           (fun succ ->
             pair2unique
             |> PairValuedMap.filter (fun (_, other_block_id) _ ->
                    succ = other_block_id)
             |> PairValuedMap.bindings
             |> List.map (fun ((original_var, _), (unique_var, var_type)) ->
                    let set_args =
                      ( unique_var,
                        StringValuedMap.find original_var original2latest )
                    in
                    Hashtbl.add setargs2type set_args var_type;
                    Bril.Instr.Set (fst set_args, snd set_args)))
           (match IntValuedMap.find_opt block_id succs with
           | None -> []
           | Some lst -> lst)
         |> List.concat
         |> fun sets -> Basic_blocks.insert_bottom_of_block sets block_instrs)
  |> List.concat
  |> fun instrs ->
  (func.args
  |> List.map (fun (arg, arg_type) ->
         let set_args = (arg ^ ".1.1", arg) in
         Hashtbl.add setargs2type set_args arg_type;
         Bril.Instr.Set (arg ^ ".1.1", arg)))
  @ instrs
  |> Bril.Func.set_instrs func
  |> fun x -> (x, Hashtbl.to_seq setargs2type |> List.of_seq)

let func_out_of_ssa (func : Bril.Func.t)
    (set2type : ((string * string) * Bril.Bril_type.t) list) : Bril.Func.t =
  func |> Bril.Func.instrs
  |> List.filter (function Bril.Instr.Get _ -> false | _ -> true)
  |> List.map (function
       | Bril.Instr.Set (arg1, arg2) ->
           let var_type = List.assoc (arg1, arg2) set2type in
           Bril.Instr.Unary ((arg1, var_type), Bril.Op.Unary.Id, arg2)
       | other_instr -> other_instr)
  |> Bril.Func.set_instrs func

let into_ssa = List.map (fun func -> func |> func_into_ssa |> fst)

let roundtrip (prog : Bril.t) : Bril.t =
  List.map
    (fun (func : Bril.Func.t) ->
      let ssa_func, set_types = func_into_ssa func in
      func_out_of_ssa ssa_func set_types)
    prog
