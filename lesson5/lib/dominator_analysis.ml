open Utils

let rec find_dominators_aux (pred_map : int list IntValuedMap.t)
    (all_blocks : IntSet.t) (map_changed : bool)
    (block2dominators : IntSet.t IntValuedMap.t) : IntSet.t IntValuedMap.t =
  if not map_changed then block2dominators
  else
    (false, block2dominators)
    |> IntSet.fold (* iterate through non-entry vertices*)
         (fun (vertex : int)
              ((map_changed, b2d_acc) : bool * IntSet.t IntValuedMap.t) ->
           (* for body starts*)
           (* union of {vertex} and {intersection of (dom[pred] for all preds)} *)
           let (vertex_dominators : IntSet.t) =
             pred_map |> IntValuedMap.find vertex
             |> List.fold_left
                  (fun intersection_acc pred ->
                    IntSet.inter intersection_acc
                      (match IntValuedMap.find_opt pred b2d_acc with
                      | None -> all_blocks
                      | Some pred_dominators -> pred_dominators))
                  all_blocks
             |> IntSet.union (IntSet.singleton vertex)
           in
           (* the map has changed if it previously changed for some other vertex,
            or if no binding currently exist for the current vertex, or if the 
            existing vertex binding and the new binding are not equal. *)
           ( (map_changed
             ||
             match IntValuedMap.find_opt vertex b2d_acc with
             | None -> true
             | Some og_vertex_dominators ->
                 vertex_dominators |> IntSet.equal og_vertex_dominators |> not),
             IntValuedMap.update vertex
               (function _ -> Some vertex_dominators)
               b2d_acc )
           (* for body ends*))
         ((* iterate through every vertex except for entry, which is indexed as 1 *)
          IntSet.filter
            (function 1 -> false | _ -> true)
            all_blocks)
    |> fun (map_changed', block2dominators') ->
    find_dominators_aux pred_map all_blocks map_changed' block2dominators'

(** [find_dominators func] is a map from each basic block to the list of basic
    blocks that dominate it. These basic blocks are formed from the contents of
    [func]. *)
let find_dominators (func : Bril.Func.t) : IntSet.t IntValuedMap.t =
  let (basic_block_preds, all_blocks) : int list IntValuedMap.t * IntSet.t =
    func |> Cfg.construct_cfg |> Cfg.preds |> fun preds ->
    ( preds,
      preds |> IntValuedMap.to_list
      |> List.filter_map (fun (k, _) -> match k with 0 -> None | _ -> Some k)
      |> IntSet.of_list )
  in
  find_dominators_aux basic_block_preds all_blocks true
    (IntValuedMap.singleton 1 (IntSet.singleton 1))

(** [dominated_of_dominators map] is a map from each basic block to the list of
    basic blocks that are dominated by it. [map] is the output of
    [find_dominators] *)
let dominated_of_dominators block2dominators =
  IntValuedMap.fold
    (fun block dominators_of_block block2dominated_acc ->
      IntSet.fold
        (fun dominator block2dominated_acc' ->
          IntValuedMap.update dominator
            (function
              | None -> Some (IntSet.singleton block)
              | Some set -> Some (IntSet.add block set))
            block2dominated_acc')
        dominators_of_block block2dominated_acc)
    block2dominators IntValuedMap.empty

(** [insert_dummy func] is a transformed Bril function, where a new entry basic
    block is inserted to represent the arguments to function [func]. If [func]
    has no arguments, then the exisitng entry block will be given a label if it
    doesn't have one already. *)
let insert_dummy (func : Bril.Func.t) : Bril.Func.t =
  match (func.args, func |> Bril.Func.instrs |> List.hd) with
  (* if func has no arguments and first block has a label, no changes *)
  | [], Bril.Instr.Label _ -> func
  (* if func has no arguments and first block isn't a label, insert label *)
  | [], _ ->
      Bril.Func.set_instrs func
        (Bril.Instr.Label "entry" :: Bril.Func.instrs func)
  (* if func has arguments {x}, then create dummy block with just the 
  instructions x : _ = id x. also, insert label for dummy block *)
  | arg_list, first_instr ->
      let function_instrs = Bril.Func.instrs func in
      let has_entry_label =
        match first_instr with Bril.Instr.Label _ -> true | _ -> false
      in
      let dummy_block_contents =
        (arg_list
        |> List.map (fun (arg : Bril.Dest.t) ->
               Bril.Instr.Unary (arg, Bril.Op.Unary.Id, fst arg)))
        @ (if has_entry_label then [] else [ Bril.Instr.Label "entry" ])
        @ function_instrs
      in
      Bril.Func.set_instrs func
        (Bril.Instr.Label "entry1" :: dummy_block_contents)

(** [get func] is a tuple, whose first element maps every basic block to a list
    of dominators of that basic block, and whose second element maps every basic
    block to the list of blocks that are dominated by that block. *)
let get_dom_maps (func : Bril.Func.t) =
  (* create a dummy entry block if the Bril function has arguments *)
  let block2dominators = find_dominators (insert_dummy func) in
  (* return a pair of maps *)
  (block2dominators, dominated_of_dominators block2dominators)

(** Prints out a map from integers to a set of integers. *)
let print_dom_analysis_map : IntSet.t IntValuedMap.t -> unit =
  IntValuedMap.iter (fun (block : int) (set : IntSet.t) ->
      string_of_int block ^ ":  "
      ^ (set |> IntSet.to_list |> List.map string_of_int |> String.concat " ")
      |> print_endline)

(** [domination_frontier_of func] is a map from each basic block B in a Bril
    function [func] to a set of basic blocks that are on domination frontier of
    B. *)
let domination_frontier_of (func : Bril.Func.t) : IntSet.t IntValuedMap.t =
  (* gets maps from each block to sets of dominated and strictly dominated blocks,
  plus a list of all blocks over which we will iterate. *)
  let (block2dominated, strict_block2dominated, all_blocks) :
      IntSet.t IntValuedMap.t * IntSet.t IntValuedMap.t * int list =
    func |> get_dom_maps |> snd |> fun dominated ->
    ( dominated,
      IntValuedMap.fold
        (fun block dominated acc ->
          IntValuedMap.add block (IntSet.remove block dominated) acc)
        dominated IntValuedMap.empty )
    |> fun (dominated, strictly_dominated) ->
    ( dominated,
      strictly_dominated,
      strictly_dominated |> IntValuedMap.bindings |> List.map fst )
  in
  (* map from each block to a list of predecessors given by the cfg *)
  let preds = func |> insert_dummy |> Cfg.construct_cfg |> Cfg.preds in
  (* For every block A and for every block B, if B is not strictly dominated by A
    and there exists a predecessor of B that is normally-dominated (i.e. reflexively)
  dominated by A, then add B to A's frontier set. *)
  List.fold_left
    (fun (block2frontier : IntSet.t IntValuedMap.t) (block_A : int) ->
      let (dominated, strictly_dominated) : IntSet.t * IntSet.t =
        ( IntValuedMap.find block_A block2dominated,
          IntValuedMap.find block_A strict_block2dominated )
      in
      IntValuedMap.add block_A
        (List.fold_left
           (fun (frontier_of_A : IntSet.t) (block_B : int) ->
             if
               IntSet.mem block_B strictly_dominated
               && IntSet.exists
                    (fun pred -> IntSet.mem pred dominated)
                    (match IntValuedMap.find_opt block_B preds with
                    | None -> IntSet.empty
                    | Some set -> IntSet.of_list set)
             then IntSet.add block_B frontier_of_A
             else frontier_of_A)
           IntSet.empty all_blocks)
        block2frontier)
    IntValuedMap.empty all_blocks

(** [compute_domination_tree f] is a data structure mapping each block in a Bril
    function [f] (indexed as an integer) to its set of blocks that it
    immediately dominates. By definition of immediate domination, we know that
    this map forms a domination tree. *)
let compute_domination_tree (func : Bril.Func.t) : IntSet.t IntValuedMap.t =
  let block_2_strictlydominated, all_blocks =
    func |> get_dom_maps |> snd |> fun block_2_dominated ->
    ( IntValuedMap.fold
        (fun (block : int) (dominated : IntSet.t)
             (acc : IntSet.t IntValuedMap.t) ->
          IntValuedMap.add block (IntSet.remove block dominated) acc)
        block_2_dominated IntValuedMap.empty,
      block_2_dominated |> IntValuedMap.bindings |> List.map fst )
  in
  (* For every block, get the set of things strictly dominated by the block. 
  Then, for every other block block', if block' dominates both block and
  the strictly dominated block, then remove the strictly dominated block
  from the set of things strictly dominated by block'. We are thenleft with the 
  set of immediately-dominated blocks, for every block. *)
  List.fold_left
    (fun (acc : IntSet.t IntValuedMap.t) (block : int) ->
      let strictly_dominated_by_block_set =
        IntValuedMap.find block block_2_strictlydominated
      in
      IntSet.fold
        (fun (strictly_dominated_by_block : int)
             (acc' : IntSet.t IntValuedMap.t) ->
          List.fold_left
            (fun (acc'' : IntSet.t IntValuedMap.t) (block' : int) ->
              let strictly_dominated_by_block'_set =
                IntValuedMap.find block' block_2_strictlydominated
              in
              if
                IntSet.mem strictly_dominated_by_block
                  strictly_dominated_by_block'_set
                && IntSet.mem block strictly_dominated_by_block'_set
              then
                IntValuedMap.update block'
                  (function
                    | None -> Some IntSet.empty
                    | Some set ->
                        Some (IntSet.remove strictly_dominated_by_block set))
                  acc''
              else acc'')
            acc' all_blocks)
        strictly_dominated_by_block_set acc)
    block_2_strictlydominated all_blocks

(* --------------------------------------------------------------------------- *)
(* |                        BEGIN TESTING FUNCTIONS                          | *)
(* --------------------------------------------------------------------------- *)

(** From a list of lists containing abstract elements, derive a list of lists of
    these abstract types up to and including some input of this type. *)
let up_to (i : 'a) : 'a list list -> 'a list list =
  List.fold_left
    (fun (acc : 'a list list) (lst : 'a list) ->
      if lst |> List.mem i |> not then acc
      else
        (lst
        |> List.fold_left
             (fun ((hit, sublst) : bool * 'a list) (elt : 'a) ->
               if hit then (hit, sublst) else (elt = i, elt :: sublst))
             (false, [])
        |> snd |> List.rev)
        :: acc)
    []

(** Given a basic block, enumerate every path within a CFG, from entry, that can
    reach the basic block. *)
let rec enumerate_paths_aux (block : int) (cfg : IntSet.t IntValuedMap.t)
    (visited : IntSet.t) (paths : int list list) : int list list =
  let visited' = IntSet.add block visited in
  IntSet.fold
    (fun (block_neighbor : int) (paths_acc : int list list) ->
      if IntSet.mem block_neighbor visited' then paths_acc
      else
        let paths_out_of_neighbor : int list list =
          enumerate_paths_aux block_neighbor cfg visited' [ [ block_neighbor ] ]
        in
        let paths_up_to_block : int list list = up_to block paths_acc in
        List.fold_left
          (fun acc path_up_to_block ->
            List.map
              (fun path_out_of_neighbor ->
                path_up_to_block @ path_out_of_neighbor)
              paths_out_of_neighbor
            @ acc)
          [] paths_up_to_block
        @ paths_acc)
    (match IntValuedMap.find_opt block cfg with
    | None -> IntSet.empty
    | Some set -> set)
    paths

let enumerate_paths (func : Bril.Func.t) : int list list =
  func |> insert_dummy |> Cfg.construct_cfg |> Cfg.succs |> fun cfg_succs ->
  [ [ 1 ] ]
  |> enumerate_paths_aux 1
       (IntValuedMap.map IntSet.of_list cfg_succs)
       IntSet.empty
  |> List.sort_uniq (fun ilst1 ilst2 ->
         match Int.compare (List.length ilst1) (List.length ilst2) with
         | 0 ->
             if ilst2 |> List.map2 Int.equal ilst1 |> List.for_all (fun x -> x)
             then 0
             else -1
         | n -> n)

let test_correctness (func : Bril.Func.t) : bool =
  let (enumerated_paths : int list list) = enumerate_paths func in
  let (block2dominated : IntSet.t IntValuedMap.t) =
    func |> get_dom_maps |> fst
  in
  let all_blocks =
    IntValuedMap.bindings block2dominated |> List.map fst |> IntSet.of_list
  in
  IntValuedMap.fold
    (fun (block : int) (dominators : IntSet.t) (prev_correct : bool) ->
      prev_correct
      && List.filter
           (fun ilst -> ilst |> List.rev |> List.hd = block)
           enumerated_paths
         |> List.map IntSet.of_list
         |> List.fold_left IntSet.inter all_blocks
         |> IntSet.equal dominators)
    block2dominated true
