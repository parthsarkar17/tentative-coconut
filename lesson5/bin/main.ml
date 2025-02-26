open Lesson5

let print_domination_analysis_func func =
  let block2dominators, block2dominated =
    Dominator_analysis.get_dom_maps func
  in
  print_endline "dominators: ";
  Dominator_analysis.print_dom_analysis_map block2dominators;
  print_endline "";
  print_endline "dominated: ";
  Dominator_analysis.print_dom_analysis_map block2dominated

let iter_func f =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> List.iter f

let _print_domination_analysis () =
  iter_func (fun func ->
      print_endline func.name;
      print_domination_analysis_func func;
      print_endline "")

let print_domination_frontier_func func =
  Dominator_analysis.(func |> domination_frontier_of |> print_dom_analysis_map);
  print_endline ""

let _print_domination_frontier () =
  iter_func (fun (func : Bril.Func.t) ->
      print_endline (Bril.Func.to_string func);
      print_domination_frontier_func func;
      print_endline "")

let _print_domination_tree () =
  iter_func (fun (func : Bril.Func.t) ->
      print_endline (Bril.Func.to_string func);
      Dominator_analysis.(
        func |> compute_domination_tree |> print_dom_analysis_map);
      print_endline "")

let _test_correctness () =
  iter_func (fun f ->
      f |> Dominator_analysis.test_correctness |> string_of_bool
      |> print_endline)

let () = _test_correctness ()
