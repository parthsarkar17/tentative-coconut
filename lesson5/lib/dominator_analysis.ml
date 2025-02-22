let rec find_dominators_aux (pred_map : int list Utils.IntValuedMap.t)
    (all_blocks : Utils.IntSet.t) (map_changed : bool)
    (block2dominators : Utils.IntSet.t Utils.IntValuedMap.t) :
    Utils.IntSet.t Utils.IntValuedMap.t =
  if map_changed then block2dominators
  else
    (false, block2dominators)
    |> Utils.IntSet.fold (* iterate through non-entry vertices*)
         (fun (vertex : int)
              ((map_changed, b2d_acc) :
                bool * Utils.IntSet.t Utils.IntValuedMap.t) ->
           (* for body starts*)
           (* union of {vertex} and {intersection of (dom[pred] for all preds)} *)
           let vertex_dominators =
             pred_map
             |> Utils.IntValuedMap.find vertex
             |> List.fold_left
                  (fun intersection_acc pred ->
                    Utils.IntSet.inter intersection_acc
                      (match Utils.IntValuedMap.find_opt pred b2d_acc with
                      | None -> all_blocks
                      | Some pred_dominators -> pred_dominators))
                  all_blocks
             |> Utils.IntSet.union (Utils.IntSet.singleton vertex)
           in
           (* the map has changed if it previously changed for some other vertex,
            or if no binding currently exist for vertexs, or if the current binding 
            and the new binding aren't equal*)
           let new_map_changed =
             map_changed
             ||
             match Utils.IntValuedMap.find_opt vertex b2d_acc with
             | None -> true
             | Some og_vertex_dominators ->
                 vertex_dominators
                 |> Utils.IntSet.equal og_vertex_dominators
                 |> not
           in
           ( new_map_changed,
             Utils.IntValuedMap.update vertex
               (function _ -> Some vertex_dominators)
               b2d_acc )
           (* for body ends*))
         ((* iterate through every vertex except for entry, which is indexed as 1 *)
          Utils.IntSet.filter
            (function 1 -> false | _ -> true)
            all_blocks)
    |> fun (x, y) -> find_dominators_aux pred_map all_blocks x y

(** [find_dominators func] is a map from each basic block to the list of basic
    blocks that dominate it. These basic blocks are formed from the contents of
    [func]. *)
let find_dominators (func : Bril.Func.t) : Utils.IntSet.t Utils.IntValuedMap.t =
  let (basic_block_preds, all_blocks) :
      int list Utils.IntValuedMap.t * Utils.IntSet.t =
    func |> Cfg.construct_cfg |> Cfg.preds |> fun preds ->
    ( preds,
      preds |> Utils.IntValuedMap.to_list
      |> List.filter_map (fun (k, _) -> match k with 0 -> None | _ -> Some k)
      |> Utils.IntSet.of_list )
  in
  find_dominators_aux basic_block_preds all_blocks false
    (Utils.IntValuedMap.singleton 1 (Utils.IntSet.singleton 1))

(** [dominated_of_dominators map] is a map from each basic block to the list of
    basic blocks that are dominated by it. [map] is the output of
    [find_dominators] *)
let dominated_of_dominators block2dominators =
  Utils.IntValuedMap.fold
    (fun block dominators_of_block block2dominated_acc ->
      Utils.IntSet.fold
        (fun dominator block2dominated_acc' ->
          Utils.IntValuedMap.update dominator
            (function
              | None -> Some (Utils.IntSet.singleton block)
              | Some set -> Some (Utils.IntSet.add block set))
            block2dominated_acc')
        dominators_of_block block2dominated_acc)
    block2dominators Utils.IntValuedMap.empty

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

let print_dom_analysis_map : Utils.IntSet.t Utils.IntValuedMap.t -> unit =
  Utils.IntValuedMap.iter (fun (block : int) (set : Utils.IntSet.t) ->
      string_of_int block ^ ":  "
      ^ (set |> Utils.IntSet.to_list |> List.map string_of_int
       |> String.concat " ")
      |> print_endline)

(** [domination_frontier_of func] is a map from each basic block B in a Bril
    function [func] to a set of basic blocks that are on domination frontier of
    B. *)
let domination_frontier_of (func : Bril.Func.t) :
    Utils.IntSet.t Utils.IntValuedMap.t =
  let block2dominated = func |> get_dom_maps |> snd in
  let succs = func |> insert_dummy |> Cfg.construct_cfg |> Cfg.succs in
  Utils.IntValuedMap.fold
    (fun (block : int) (set_of_dominated : Utils.IntSet.t)
         (frontier_map : Utils.IntSet.t Utils.IntValuedMap.t) ->
      Utils.IntValuedMap.update block
        ( Utils.IntSet.fold
            (fun (indiv_dominated_block : int) ->
              Utils.IntSet.union
                (List.fold_left
                   (fun (non_dominated_succs : Utils.IntSet.t) (succ : int) ->
                     if set_of_dominated |> Utils.IntSet.mem succ |> not then
                       Utils.IntSet.add succ non_dominated_succs
                     else non_dominated_succs)
                   Utils.IntSet.empty
                   (match
                      Utils.IntValuedMap.find_opt indiv_dominated_block succs
                    with
                   | None -> []
                   | Some l -> l)))
            set_of_dominated Utils.IntSet.empty
        |> fun block_frontier_set -> function
          | _ -> Some block_frontier_set )
        frontier_map)
    block2dominated Utils.IntValuedMap.empty

type domination_tree = Block of (int option * domination_tree list)

let compute_domination_tree (func : Bril.Func.t) =
  let block_2_strictlydominated, list_of_blocks =
    func |> get_dom_maps |> snd |> fun block_2_dominated ->
    ( Utils.IntValuedMap.fold
        (fun (block : int) (dominated : Utils.IntSet.t)
             (acc : Utils.IntSet.t Utils.IntValuedMap.t) ->
          Utils.IntValuedMap.add block (Utils.IntSet.remove block dominated) acc)
        block_2_dominated Utils.IntValuedMap.empty,
      block_2_dominated |> Utils.IntValuedMap.bindings |> List.map fst )
  in
  let block_2_immediatelydominated =
    List.fold_left
      (fun (acc : Utils.IntSet.t Utils.IntValuedMap.t) (block : int) ->
        let strictly_dominated_by_block_set =
          Utils.IntValuedMap.find block block_2_strictlydominated
        in
        Utils.IntSet.fold
          (fun (strictly_dominated_by_block : int)
               (acc' : Utils.IntSet.t Utils.IntValuedMap.t) ->
            List.fold_left
              (fun (acc'' : Utils.IntSet.t Utils.IntValuedMap.t) (block' : int)
                 ->
                let strictly_dominated_by_block'_set =
                  Utils.IntValuedMap.find block' block_2_strictlydominated
                in
                if
                  Utils.IntSet.mem strictly_dominated_by_block
                    strictly_dominated_by_block'_set
                  && Utils.IntSet.mem block strictly_dominated_by_block'_set
                then
                  Utils.IntValuedMap.update block'
                    (function
                      | None -> Some Utils.IntSet.empty
                      | Some set ->
                          Some
                            (Utils.IntSet.remove strictly_dominated_by_block set))
                    acc''
                else acc'')
              acc' list_of_blocks)
          strictly_dominated_by_block_set acc)
      block_2_strictlydominated list_of_blocks
  in
  block_2_immediatelydominated
