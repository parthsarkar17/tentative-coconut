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
  let rec reaches_tail (visited : IntSet.t) (graph : int list IntValuedMap.t) :
      int list -> IntSet.t = function
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

(** [loop_invariant_instrs_of] computes a set of instruction IDs (a tuple
    consisting of a block ID and a local-to-block instruction ID) to represent
    the set of instructions that are loop invariant. *)
let loop_invariant_instrs_of (basic_blocks : Basic_blocks.t)
    (reaching_defs : Reaching_analysis.DefinitionSet.t IntValuedMap.t)
    (natural_loop : loop) : PairSet.t =
  let instrs_in_loop =
    natural_loop.blocks
    |> List.concat_map (fun (block_id : int) ->
           basic_blocks |> IntValuedMap.find block_id
           |> List.map (fun (instr_id, instr) -> (block_id, instr_id, instr)))
  in
  (* recurse till convergence *)
  let rec compute_loop_invariants (loop_invariants : PairSet.t) : PairSet.t =
    let new_loop_invariants =
      List.fold_left
        (fun (loop_invariants : PairSet.t)
             ((block_id, instr_id, instr) : int * int * Bril.Instr.t) ->
          (* The definitions reaching this [instr] are the reaching defs from 
          previous blocks (i.e. the in-flow to block [block_id], united with the 
          definitions from [block_id] itself, up to and not including [instr]). *)
          let defs_reaching_instr =
            Reaching_analysis.DefinitionSet.union
              (IntValuedMap.find block_id reaching_defs)
              (basic_blocks |> IntValuedMap.find block_id
              |> List.filter_map (fun (instr_id', instr') ->
                     match (instr_id' < instr_id, Bril.Instr.dest instr') with
                     | false, _ | _, None -> None
                     | true, Some ((dest_var, _) : Bril.Dest.t) ->
                         Some (block_id, instr_id', dest_var))
              |> List.fold_left
                   (fun acc elt -> Reaching_analysis.DefinitionSet.add elt acc)
                   Reaching_analysis.DefinitionSet.empty)
          in
          let instr_is_loop_invariant : bool =
            instr |> Bril.Instr.args |> function
            | [] -> false
            | args ->
                args
                |> List.for_all (fun (arg : string) ->
                       let reaching_defs_of_arg =
                         Reaching_analysis.DefinitionSet.filter
                           (fun (_, _, dest) -> dest = arg)
                           defs_reaching_instr
                       in
                       (* first way to have loop invariance *)
                       let all_reaching_defs_outside_loop =
                         Reaching_analysis.DefinitionSet.for_all
                           (fun (block_id', _, _) ->
                             not (List.mem block_id' natural_loop.blocks))
                           reaching_defs_of_arg
                       in
                       (* second way to have loop invariance *)
                       let one_loop_invariant_def =
                         Reaching_analysis.DefinitionSet.cardinal
                           reaching_defs_of_arg
                         = 1
                         && Reaching_analysis.DefinitionSet.for_all
                              (fun (block_id', instr_id', _) ->
                                PairSet.mem (block_id', instr_id')
                                  loop_invariants)
                              reaching_defs_of_arg
                       in
                       (* TWO-WAY ENTRY TO MARKING SOMETHING AS LOOP INVARIANT *)
                       all_reaching_defs_outside_loop || one_loop_invariant_def)
          in
          if instr_is_loop_invariant then
            let new_loop_invariants =
              PairSet.add (block_id, instr_id) loop_invariants
            in
            new_loop_invariants
          else loop_invariants)
        PairSet.empty instrs_in_loop
    in
    (* iterate till convergence *)
    if not (PairSet.equal new_loop_invariants loop_invariants) then
      compute_loop_invariants new_loop_invariants
    else loop_invariants
  in
  compute_loop_invariants PairSet.empty

(** [insert_preheaders func] is a [Bril.Func.t] where, for every natural loop in
    the CFG of [func], we insert a preheader to the entry of the natural loop in
    order to have a destination in which to place LICM-ified instructions. This
    preheader is placed directly before the entry point of the natural loop
    (i.e. block_id - 1). *)
let insert_preheaders (func : Bril.Func.t) : Bril.Func.t =
  func |> natural_loops_of
  |> List.fold_left
       (fun ((func, loop_id) : Bril.Func.t * int) (natural_loop : loop) ->
         let _, dominator = natural_loop.backedge in
         let blocks_in_natural_loop = natural_loop.blocks in
         let basic_blocks = Basic_blocks.form_blocks func in
         let preds = Cfg.(func |> construct_cfg |> preds) in
         match IntValuedMap.find_opt dominator preds with
         | None | Some [] -> (func, loop_id + 1)
         | _ ->
             let dominator_label =
               match
                 basic_blocks |> IntValuedMap.find dominator |> List.hd |> snd
               with
               | Bril.Instr.Label lbl -> lbl
               | _ ->
                   Stdlib.failwith
                     "Unreachable: No label for the dominating block in this \
                      backedge. "
             in
             let preheader_label = "preheader" ^ string_of_int loop_id in
             let loop_preheader =
               [
                 Bril.Instr.Label preheader_label;
                 Bril.Instr.Jmp dominator_label;
               ]
             in
             (* set all jumps to dominator label to instead jump to preheader label,
             except for those within the natural loop. *)
             let instrs_before_preheader, instrs_after_preheader =
               basic_blocks |> IntValuedMap.bindings
               |> List.concat_map (fun (block_id, identified_instrs) ->
                      List.map
                        (fun (instr_id, instr) -> (block_id, instr_id, instr))
                        identified_instrs)
               |> List.map (fun (block_id, instr_id, instr) ->
                      let new_instr =
                        if List.mem block_id blocks_in_natural_loop then instr
                        else
                          (* ungainly logic that just swaps labels *)
                          match instr with
                          | Bril.Instr.Jmp some_label ->
                              if some_label = dominator_label then
                                Bril.Instr.Jmp preheader_label
                              else instr
                          | Bril.Instr.Br (cond, some_label1, some_label2) ->
                              let label1_equal =
                                some_label1 = dominator_label
                              in
                              let label2_equal =
                                some_label2 = dominator_label
                              in
                              if label1_equal && label2_equal then
                                Bril.Instr.Br
                                  (cond, preheader_label, preheader_label)
                              else if label1_equal then
                                Bril.Instr.Br
                                  (cond, preheader_label, some_label2)
                              else if label2_equal then
                                Bril.Instr.Br
                                  (cond, some_label1, preheader_label)
                              else instr
                          | _ -> instr
                      in
                      (block_id, instr_id, new_instr))
               |> List.partition (fun (block_id, _, _) -> block_id < dominator)
             in
             let strip_indexes = List.map (fun (_, _, instr) -> instr) in
             ( Bril.Func.set_instrs func
                 (* place loop preheader *)
                 (strip_indexes instrs_before_preheader
                 @ loop_preheader
                 @ strip_indexes instrs_after_preheader),
               loop_id + 1 ))
       (func, 0)
  |> fst

(** [transform func] is a [Bril.Func.t] that has undergone the LICM pass. *)
let transform (func : Bril.Func.t) : Bril.Func.t =
  let func_with_preheaders = insert_preheaders func in
  let basic_blocks, reaching_defs, natural_loops, block2dominated, succs_map =
    ( Basic_blocks.form_blocks func_with_preheaders,
      func_with_preheaders |> Reaching_analysis.reaching_defs_of |> fst,
      natural_loops_of func_with_preheaders,
      func_with_preheaders |> Dominator_analysis.get_dom_maps |> snd,
      func_with_preheaders |> Cfg.construct_cfg |> Cfg.succs )
  in
  natural_loops
  |> List.fold_left
       (fun (basic_blocks_acc : Basic_blocks.t) (natural_loop : loop) ->
         natural_loop |> loop_invariant_instrs_of basic_blocks_acc reaching_defs
         |> fun loop_invariant_instrs ->
         PairSet.fold
           (fun (block_id, instr_id) basic_blocks_acc_acc ->
             let definition =
               basic_blocks_acc_acc |> IntValuedMap.find block_id
               |> List.assoc instr_id
             in
             let blocks_where_used =
               Basic_blocks.blocks_where_used definition basic_blocks_acc_acc
             in
             let all_dominated_blocks =
               IntValuedMap.find block_id block2dominated
             in
             (* first condition *)
             let definition_dominates_uses =
               IntSet.subset blocks_where_used all_dominated_blocks
             in
             (* second condition *)
             let is_unique_definition =
               natural_loop.blocks
               |> List.fold_left
                    (fun (num_defs : int) (loop_block_id : int) ->
                      basic_blocks_acc
                      |> IntValuedMap.find loop_block_id
                      |> List.fold_left
                           (fun num_defs' (_, instr) ->
                             num_defs'
                             +
                             match Bril.Instr.dest instr with
                             | None -> 0
                             | Some (dest_var, _) ->
                                 if
                                   Bril.Instr.dest definition |> Option.get
                                   |> fst |> ( = ) dest_var
                                 then 1
                                 else 0)
                           num_defs)
                    0
               |> ( = ) 1
             in
             (* third condition *)
             let definition_dominates_loop_exits =
               natural_loop.blocks
               |> List.concat_map (fun (loop_block_id : int) ->
                      succs_map |> IntValuedMap.find_opt loop_block_id
                      |> function
                      | None -> []
                      | Some list_of_succs ->
                          List.filter
                            (fun (succ : int) ->
                              not (List.mem succ natural_loop.blocks))
                            list_of_succs)
               |> List.for_all (fun (loop_exit : int) ->
                      IntSet.mem loop_exit all_dominated_blocks)
             in
             (* THREE-PART CONDITION TO MOVE INSTRUCTION HERE *)
             if
               definition_dominates_uses && is_unique_definition
               && definition_dominates_loop_exits
             then
               basic_blocks_acc_acc
               (* move instruction to preheader *)
               |> Basic_blocks.insert_into_block definition
                    (snd natural_loop.backedge - 1)
                  (* remove instruction from loop *)
               |> Basic_blocks.remove_from_block (block_id, instr_id)
             else basic_blocks_acc_acc)
           loop_invariant_instrs basic_blocks_acc)
       basic_blocks
  |> Basic_blocks.just_blocks |> List.flatten
  |> Bril.Func.set_instrs func_with_preheaders

(* --------------------------------------------------------------------------- *)
(*                          BEGIN TESTING FUNCTIONS                            *)
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

let print_reaching_defs (func : Bril.Func.t) : unit =
  print_endline func.name;
  func |> Reaching_analysis.reaching_defs_of |> fst
  |> IntValuedMap.iter (fun block_id set ->
         string_of_int block_id ^ ": "
         ^ (set |> Reaching_analysis.DefinitionSet.to_list
           |> List.map (fun (x, y, z) ->
                  "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ z
                  ^ ")")
           |> String.concat "; ")
         |> print_endline);
  print_endline ""

let print_insert_preheaders (func : Bril.Func.t) : unit =
  func |> insert_preheaders |> Bril.Func.to_string |> print_endline
