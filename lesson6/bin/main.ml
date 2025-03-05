let iter_func f =
  In_channel.stdin |> In_channel.input_all |> Yojson.Basic.from_string
  |> Bril.from_json |> List.iter f

let () = iter_func Lesson6.Ssa.test_fn
