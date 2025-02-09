(** A data type for the elements of each lattice member in the powerset lattice
    where each set represents the reaching definitions for a basic block. *)
module Definition = struct
  type t = int * int * string

  let to_string : t -> string = function
    | b, i, dest ->
        "(" ^ string_of_int b ^ ", " ^ string_of_int i ^ ", " ^ dest ^ ")"

  let compare (b1, i1, d1) (b2, i2, d2) =
    match Int.compare b1 b2 with
    | 0 -> ( match Int.compare i1 i2 with 0 -> String.compare d1 d2 | m -> m)
    | n -> n
end

(** Fill out the data flow analysis form for reaching analysis. *)
module ReachingAnalysisTemplate : Dataflow_analysis.LATTICE = struct
  module DefinitionSet = Set.Make (Definition)

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
