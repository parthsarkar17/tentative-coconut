open Utils

type loop = { backedge : int * int; blocks : int list }
(** A natural loop contains a set of basic blocks composing the loop, plus
    direct access to the backedge around which the loop is built. *)

(** [backedges_of f] is a list of integer tuples each representing a backedge in
    the Bril function [f], where, within each backedge, the first integer is the
    identifier of the basic block that is dominated and the second integer is
    the identifier of the basic block that is dominating. *)
let backedges_of (func : Bril.Func.t) : (int * int) list =
  let block2dominated = func |> Dominator_analysis.get_dom_maps |> snd in
  let block2succs = Cfg.(func |> construct_cfg |> succs) in
  IntValuedMap.fold
    (fun (block : int) (block_succs : int list) (acc : (int * int) list) ->
      block_succs
      |> List.filter_map (fun (block_succ : int) ->
             match IntValuedMap.find_opt block_succ block2dominated with
             | None -> None
             | Some (dom_by_succ : IntSet.t) ->
                 if IntSet.mem block dom_by_succ then Some (block, block_succ)
                 else None)
      |> ( @ ) acc)
    block2succs []

(** [natural_loops_of f] is a list of values of type [loop], representing the
    natural loops found in the Bril function [f]. *)
let natural_loops_of (func : Bril.Func.t) : loop list =
  (* performs dfs to find all parts of the loop that reach the tail *)
  let rec reaches_tail visited graph = function
    | [] -> visited
    | h :: t ->
        if IntSet.mem h visited then reaches_tail visited graph t
        else
          reaches_tail (IntSet.add h visited) graph
            (match IntValuedMap.find_opt h graph with
            | None -> t
            | Some children -> children @ t)
  in
  func |> backedges_of
  |> List.map (fun ((tail, head) : int * int) ->
         let transformed_preds =
           func |> Cfg.construct_cfg |> Cfg.preds
           |> IntValuedMap.map (List.filter (( <> ) head))
           |> IntValuedMap.remove head
         in
         {
           backedge = (tail, head);
           blocks =
             head
             :: ([ tail ]
                |> reaches_tail IntSet.empty transformed_preds
                |> IntSet.to_list);
         })

(* --------------------------------------------------------------------------- *)
(*                         BEGIN TESTING FUNCTIONS                             *)
(* --------------------------------------------------------------------------- *)

let print_backedges (func : Bril.Func.t) : unit =
  func |> backedges_of
  |> List.map (fun (a, b) ->
         "(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ")")
  |> String.concat ", " |> print_endline

let print_natural_loops (func : Bril.Func.t) : unit =
  func |> natural_loops_of
  |> List.map (fun { blocks; _ } ->
         blocks |> List.map string_of_int |> String.concat " ")
  |> String.concat "\n" |> print_endline

let transform (func : Bril.Func.t) : Bril.Func.t = func
