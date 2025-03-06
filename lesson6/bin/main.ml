let get_results () =
  "./perf-data.csv" |> Csv.load |> Lesson6.Analyze_stats.produce_results

let roundtrip () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> Lesson6.Ssa.roundtrip |> Bril.to_json
  |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout

let just_ssa () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> Lesson6.Ssa.into_ssa |> Bril.to_json
  |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout

let () =
  try
    if Sys.argv.(1) = "rt" then roundtrip ()
    else if Sys.argv.(1) = "stats" then get_results ()
    else just_ssa ()
  with _ -> just_ssa ()
