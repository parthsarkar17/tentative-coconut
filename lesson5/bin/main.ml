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

let print_domination_analysis () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> List.iter (fun (func : Bril.Func.t) ->
         print_endline func.name;
         print_domination_analysis_func func;
         print_endline "")

let () = print_domination_analysis ()
