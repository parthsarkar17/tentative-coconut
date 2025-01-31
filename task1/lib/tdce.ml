module ArgumentSet = Set.Make (String)

(** Implements global dead code elimination. [input_prog] is the bril program
    we're operating on, and [prev_transformed] is a flag saying if we got rid of
    an instruction in the previous interation (i.e. if we made a change to the
    program the last time around). If we did, it's important to check if any
    other instructions have become dead, but if we didn't then we can feel free
    to stop checking for unused instructions. *)
let rec elim_global_unused_assigns (prog : Bril.t) : Bril.t =
  (* create a flag for the current iteration indicating whether the code transformed *)
  let create_flag : unit -> bool ref = fun () -> ref false in
  let changed_something : bool ref = create_flag () in
  prog
  |> List.map
       (* transform every function within a bril program *)
       (fun (func : Bril.Func.t) ->
         let instrs = Bril.Func.instrs func in
         instrs
         |> List.fold_left
              (fun (used : Bril.Instr.arg list) (instr : Bril.Instr.t) ->
                (* enumerate instructions and add arguments to a set of used variables*)
                match instr with
                | Bril.Instr.Unary (_, _, arg)
                | Bril.Instr.Br (arg, _, _)
                | Bril.Instr.Guard (arg, _)
                | Bril.Instr.Alloc (_, arg)
                | Bril.Instr.Free arg
                | Bril.Instr.Load (_, arg) ->
                    arg :: used
                | Bril.Instr.Binary (_, _, arg1, arg2)
                | Bril.Instr.Store (arg1, arg2)
                | Bril.Instr.PtrAdd (_, arg1, arg2) ->
                    arg1 :: arg2 :: used
                | Bril.Instr.Call (_, _, arg_list) | Bril.Instr.Print arg_list
                  ->
                    arg_list @ used
                | Bril.Instr.Ret arg_opt -> (
                    match arg_opt with None -> used | Some arg -> arg :: used)
                | Bril.Instr.Phi (_, labeled_args) ->
                    labeled_args |> List.map snd |> ( @ ) used
                | _ -> used)
              []
         |> ArgumentSet.of_list
         |> fun (used : ArgumentSet.t) ->
         instrs
         (* delete instructions that have unused destination variables *)
         |> List.filter (fun (instr : Bril.Instr.t) ->
                (* lay out cases where destination exists, and determine if it's used*)
                match instr with
                | Bril.Instr.Const (dst, _)
                | Bril.Instr.Binary (dst, _, _, _)
                | Bril.Instr.Unary (dst, _, _)
                | Bril.Instr.Phi (dst, _)
                | Bril.Instr.Alloc (dst, _)
                | Bril.Instr.Load (dst, _)
                | Bril.Instr.PtrAdd (dst, _, _) ->
                    let inst_used = ArgumentSet.mem (fst dst) used in
                    (* set flag if we transformed the code *)
                    if not inst_used then changed_something := true else ();
                    inst_used
                (* of course, keep all instructions that don't have destinations *)
                | _ -> true)
         |> Bril.Func.set_instrs func)
  |> fun (transformed_prog : Bril.t) ->
  if !changed_something then elim_global_unused_assigns transformed_prog
  else prog
