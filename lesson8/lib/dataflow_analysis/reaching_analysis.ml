open Utils

(** A data type for the elements of each lattice member in the powerset lattice
    where each set represents the reaching definitions for a basic block. *)
module Definition = struct
  type t = int * int * string
  (** [t] is the abstract type for a "definition" to perform our reaching
      definition analysis.*)

  let to_string : t -> string = function
    | b, i, dest ->
        "(" ^ string_of_int b ^ ", " ^ string_of_int i ^ ", " ^ dest ^ ")"

  let compare (b1, i1, d1) (b2, i2, d2) =
    match Int.compare b1 b2 with
    | 0 -> ( match Int.compare i1 i2 with 0 -> String.compare d1 d2 | m -> m)
    | n -> n
end

module DefinitionSet = Set.Make (Definition)

(** Fill out the data flow analysis form for reaching analysis. *)
module ReachingAnalysisTemplate = struct
  module DefinitionSet = DefinitionSet

  type t = DefinitionSet.t

  let empty = DefinitionSet.empty

  let init (func : Bril.Func.t) : t =
    func.args
    |> List.fold_left
         (fun (acc, i) (dest_name, _) ->
           (DefinitionSet.add (0, i, dest_name) acc, i + 1))
         (DefinitionSet.empty, 0)
    |> fst

  let equal = DefinitionSet.equal

  let transfer (block : (int * int * Bril.Instr.t) list) (in_b : t) : t =
    block
    (* get the definitions defined in this block *)
    |> List.fold_left
         (fun acc (b, i, instr) ->
           match Bril.Instr.dest instr with
           | None -> acc
           | Some (dest_name, _) -> DefinitionSet.add (b, i, dest_name) acc)
         DefinitionSet.empty
    (* unite them with the definitions remaining after removing those killed in this block *)
    |> DefinitionSet.union
         (List.fold_left
            (fun acc (_, _, instr) ->
              match Bril.Instr.dest instr with
              | None -> acc
              | Some (instr_dest_name, _) ->
                  DefinitionSet.filter
                    (fun (_, _, elt_dest_name) ->
                      elt_dest_name != instr_dest_name)
                    acc)
            in_b block)

  let merge = DefinitionSet.union

  let to_string set =
    DefinitionSet.to_list set
    |> List.map (fun mem -> Definition.to_string mem)
    |> List.fold_left (fun acc str_of_mem -> acc ^ str_of_mem ^ "  ") ""
end

module ReachingAnalysis =
  Dataflow_analysis.DataFlowAnalysis (ReachingAnalysisTemplate)

(** Given a Bril function, returns a tuple of maps, each mapping a block index
    to the in-flow or out-flow of the dataflow analysis, respectively. *)
let reaching_defs_of (func : Bril.Func.t) :
    DefinitionSet.t IntValuedMap.t * DefinitionSet.t IntValuedMap.t =
  (* out block contains a dummy set of out flow definitions out of the dummy first block;
       get rid of it for pretty printing *)
  let in_blocks, out_blocks =
    func |> ReachingAnalysis.worklist_algorithm |> fun (x, y) ->
    (x, ReachingAnalysis.IntValuedMap.remove 0 y)
  in
  (* get rid of dummy definitions (i.e. definitions "defined" in block 0, which actually
      is itself a dummy block)*)
  let remove_dummy_defs =
    ReachingAnalysis.IntValuedMap.map
      (ReachingAnalysisTemplate.DefinitionSet.filter (function
        | 0, _, _ -> false
        | _ -> true))
  in
  (remove_dummy_defs in_blocks, remove_dummy_defs out_blocks)
