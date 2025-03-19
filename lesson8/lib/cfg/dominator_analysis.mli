
val get_dom_maps : Bril.Func.t -> ((Utils.IntSet.t Utils.IntValuedMap.t) * (Utils.IntSet.t Utils.IntValuedMap.t))

val print_dom_analysis_map : Utils.IntSet.t Utils.IntValuedMap.t -> unit

val domination_frontier_of :  Bril.Func.t -> Utils.IntSet.t Utils.IntValuedMap.t

val domination_tree_of : Bril.Func.t -> Utils.IntSet.t Utils.IntValuedMap.t