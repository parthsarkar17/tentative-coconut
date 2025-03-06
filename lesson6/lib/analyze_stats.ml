type data = {
  max : float;
  min : float;
  mean : float;
  stdev : float;
  median : float;
}

let string_of_data data =
  "\nmaximum: " ^ string_of_float data.max ^ "\n" ^ "minimum: "
  ^ string_of_float data.min ^ "\n" ^ "mean: " ^ string_of_float data.mean
  ^ "\n" ^ "stdev: " ^ string_of_float data.stdev ^ "\n" ^ "median: "
  ^ string_of_float data.median
  ^ "\n"

let extract_data csv =
  let group_into_twos lst =
    let rec group_into_twos_aux acc lst =
      match lst with
      | [] -> Some acc
      | _ :: [] -> failwith "not an even number of elements"
      | a :: b :: t ->
          group_into_twos_aux ((b, a) :: acc) t (* get percent decrease *)
    in
    group_into_twos_aux [] lst
  in
  csv
  |> List.filter (fun row ->
         row |> List.exists (fun str -> str = "benchmark") |> not)
  |> List.map (fun row -> row |> List.rev |> List.hd)
  |> group_into_twos |> Option.get
  |> List.map (function v1, v2 ->
         print_endline (v1 ^ ", " ^ v2);
         let fl_v2 = float_of_string v2 in
         let fl_v1 = float_of_string v1 in
         (fl_v1 -. fl_v2) /. fl_v2)

let compute_median lst =
  let rec compute_median_aux = function
    | [] -> failwith "unreachable"
    | [ v ] -> v
    | [ v1; v2 ] -> (v2 -. v1) /. 2.
    | _ :: t -> t |> List.rev |> List.tl |> List.rev |> compute_median_aux
  in
  lst |> List.sort Float.compare |> compute_median_aux

let compute_data lst =
  let lst = List.map (fun f -> f *. 100.) lst in
  let n = lst |> List.length |> float_of_int in
  let mean = List.fold_left ( +. ) 0. lst /. n in
  {
    max = List.fold_left Float.max 0. lst;
    min = List.fold_left Float.min 100. lst;
    mean;
    stdev =
      Float.sqrt
        (List.fold_left
           (fun acc elt ->
             let x = elt -. mean in
             acc +. (x *. x))
           0. lst
        /. n);
    median = compute_median lst;
  }

let probability_of_improvement percent datapoints =
  datapoints
  |> List.fold_left (fun acc elt -> if elt >= percent then acc + 1 else acc) 0
  |> float_of_int
  |> fun x -> x /. (datapoints |> List.length |> float_of_int)

let normalize lst =
  let data = compute_data lst in
  List.map (fun f -> (f -. data.mean) /. data.stdev) lst

let produce_results csv =
  csv |> extract_data |> compute_data |> string_of_data |> print_endline
