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

(** [get func] is a tuple, whose first element maps every basic block to a list
    of dominators of that basic block, and whose second element maps every basic
    block to the list of blocks that are dominated by that block. *)
let get func =
  let block2dominators = find_dominators func in
  (block2dominators, dominated_of_dominators block2dominators)
