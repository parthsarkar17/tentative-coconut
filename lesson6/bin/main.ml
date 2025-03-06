let _iter_func f =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> List.iter f

let () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json
  |> (try
        if Sys.argv.(1) = "rt" then Lesson6.Ssa.roundtrip
        else Lesson6.Ssa.into_ssa
      with _ -> Lesson6.Ssa.into_ssa)
  |> Bril.to_json |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout
