module Pair = struct
  type t = int * int

  let compare (a1, b1) (a2, b2) =
    match Int.compare a1 a2 with 0 -> Int.compare b1 b2 | n -> n
end

module IntValuedMap = Map.Make (Int)
module StringValuedMap = Map.Make (String)
module StringSet = Set.Make (String)
module IntSet = Set.Make (Int)
module PairSet = Set.Make (Pair)
