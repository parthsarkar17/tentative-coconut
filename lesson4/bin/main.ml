open Lesson4

module ReachingAnalysis =
  Dataflow_analysis.DataFlowAnalysis (Reaching_analysis.ReachingAnalysisTemplate)

(** For a given Bril program, print out the reaching definitions for each basic
    block within each function. *)
let _test_reaching_def () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map (fun func ->
         (func, Reaching_analysis.consolidate_reaching_defs func))
  |> List.iter (fun ((func, (in_block_map, out_block_map)) : Bril.Func.t * _) ->
         print_endline ("func: " ^ func.name);
         let print_block_map block_map =
           block_map |> ReachingAnalysis.IntValuedMap.to_seq
           |> Seq.iter (fun (block_idx, reaching_defs) ->
                  print_endline
                    (string_of_int block_idx ^ ": "
                    ^ String.concat ", " reaching_defs))
         in
         print_endline "in: ";
         print_block_map in_block_map;
         print_endline "out: ";
         print_block_map out_block_map);
  print_endline ""

(** For a given Bril program, for each basic block, print out a set of variables
    that can provably only take a single, constant value.*)
let _test_constant_propagation () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.iter (fun (func : Bril.Func.t) ->
         print_endline ("func: " ^ func.name);
         func |> Constant_propagation.provable_constants
         |> List.iter (fun (block_idx, single_valued_consts) ->
                print_endline
                  (string_of_int block_idx ^ ": "
                  ^ Constant_propagation.ConstantPropagationLattice.to_string
                      single_valued_consts)))

let () = _test_reaching_def ()
