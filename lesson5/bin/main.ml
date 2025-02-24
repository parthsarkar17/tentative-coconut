open Lesson5

let print_dom_analysis_map : Utils.IntSet.t Utils.IntValuedMap.t -> unit =
  Utils.IntValuedMap.iter (fun block set ->
      string_of_int block ^ ":  "
      ^ (set |> Utils.IntSet.to_list |> List.map string_of_int
       |> String.concat " ")
      |> print_endline)

let print_domination_analysis_func func =
  let block2dominators, block2dominated =
    Dominator_analysis.get_dom_maps func
  in
  print_endline "dominators: ";
  print_dom_analysis_map block2dominators;
  print_endline "";
  print_endline "dominated: ";
  print_dom_analysis_map block2dominated

let iter_func f =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> List.iter f

let _print_domination_analysis () =
  iter_func (fun func ->
      print_endline func.name;
      print_domination_analysis_func func;
      print_endline "")

let print_domination_frontier_func func =
  func |> Dominator_analysis.domination_frontier_of |> print_dom_analysis_map;
  print_endline ""

let _print_domination_frontier () =
  iter_func (fun (func : Bril.Func.t) ->
      print_endline (Bril.Func.to_string func);
      print_domination_frontier_func func;
      print_endline "")

let _print_domination_tree () =
  iter_func (fun (func : Bril.Func.t) ->
      print_endline (Bril.Func.to_string func);
      print_dom_analysis_map (Dominator_analysis.compute_domination_tree func);
      print_endline "")

let _brute_force_enumerate_paths () =
  iter_func (fun f ->
      f |> Dominator_analysis.test_correctness |> string_of_bool
      |> print_endline)

let () = _brute_force_enumerate_paths ()
