open Lesson4

module ReachingAnalysis =
  Dataflow_analysis.DataFlowAnalysis (Reaching_analysis.ReachingAnalysisTemplate)

(** For a given Bril program, print out the reaching definitions for each basic
    block within each function. *)
let _test_reaching_def () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map (fun func -> (func, ReachingAnalysis.worklist_algorithm func))
  |> List.iter (fun ((func, (in_block_map, _)) : Bril.Func.t * _) ->
         print_endline ("func: " ^ func.name);
         in_block_map |> ReachingAnalysis.IntValuedMap.to_seq
         |> Seq.iter (fun (block_idx, reaching_defs) ->
                print_endline
                  (string_of_int block_idx ^ ": "
                  ^ Reaching_analysis.ReachingAnalysisTemplate.to_string
                      reaching_defs)));
  print_endline ""

let test_constant_propagation () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.map (fun func ->
         (func, Constant_propagation.ConstantPropagation.worklist_algorithm func))
  |> List.iter (fun ((func, (in_block_map, _)) : Bril.Func.t * _) ->
         print_endline ("func: " ^ func.name);
         in_block_map
         |> Constant_propagation.ConstantPropagation.IntValuedMap.to_seq
         |> Seq.iter (fun (block_idx, reaching_defs) ->
                print_endline
                  (string_of_int block_idx ^ ": "
                  ^ Constant_propagation.ConstantPropagationTemplate.to_string
                      reaching_defs)))

let () = test_constant_propagation ()
