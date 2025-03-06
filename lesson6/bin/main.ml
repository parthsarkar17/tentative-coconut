let _iter_func f =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> List.iter f

let () =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> Lesson6.Ssa.roundtrip |> Bril.to_json
  |> Yojson.Basic.to_string
  |> Out_channel.output_string Out_channel.stdout
