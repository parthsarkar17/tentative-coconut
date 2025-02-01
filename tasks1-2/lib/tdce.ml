module ArgumentSet = Set.Make (String)
module InstructionSet = Set.Make (Int)
module VariableMap = Map.Make (String)

(** A function to lazily create pointer to a flag, so we can use a new flag at
    every recursive call. *)
let create_flag () : bool ref = ref false

(** [get_instr_dest instr] is an option wrapping the destination field of an
    instruction. Optional because not every instruction has a destination (e.g.
    `print`) *)
let get_instr_dest : Bril.Instr.t -> Bril.Dest.t option = function
  | Bril.Instr.Const (dst, _)
  | Bril.Instr.Binary (dst, _, _, _)
  | Bril.Instr.Unary (dst, _, _)
  | Bril.Instr.Phi (dst, _)
  | Bril.Instr.Alloc (dst, _)
  | Bril.Instr.Load (dst, _)
  | Bril.Instr.PtrAdd (dst, _, _) ->
      Some dst
  | _ -> None

(** A type to enumerate the kinds of arguments that Bril instructions have *)
type instr_arg_fmt =
  | None
  | One of Bril.Instr.arg
  | Two of Bril.Instr.arg * Bril.Instr.arg
  | List of Bril.Instr.arg list

(** [get_instr_args instr] is a `instr_arg_fmt` value representing the arguments
    used in [instr] *)
let get_instr_args : Bril.Instr.t -> instr_arg_fmt = function
  | Bril.Instr.Unary (_, _, arg)
  | Bril.Instr.Br (arg, _, _)
  | Bril.Instr.Guard (arg, _)
  | Bril.Instr.Alloc (_, arg)
  | Bril.Instr.Free arg
  | Bril.Instr.Load (_, arg) ->
      One arg
  | Bril.Instr.Binary (_, _, arg1, arg2)
  | Bril.Instr.Store (arg1, arg2)
  | Bril.Instr.PtrAdd (_, arg1, arg2) ->
      Two (arg1, arg2)
  | Bril.Instr.Call (_, _, arg_list) | Bril.Instr.Print arg_list ->
      List arg_list
  | Bril.Instr.Phi (_, labeled_args) -> List (List.map snd labeled_args)
  | Bril.Instr.Ret arg_opt -> (
      match arg_opt with None -> None | Some arg -> One arg)
  | _ -> None

(** Implements global dead code elimination. [input_prog] is the bril program
    we're operating on. If we removed some instructions in one iteration, it's
    important to check if any other instructions have become dead in the next
    iteration; if we didn't, we can feel free to stop checking for assignments
    instructions. *)
let rec elim_global_unused_assigns (prog : Bril.t) : Bril.t =
  (* create a flag for the current iteration indicating whether the code transformed *)
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
                match get_instr_args instr with
                | None -> used
                | One arg -> arg :: used
                | Two (arg1, arg2) -> arg1 :: arg2 :: used
                | List args -> args @ used)
              []
         |> ArgumentSet.of_list
         |> fun (used : ArgumentSet.t) ->
         instrs
         (* delete instructions that have unused destination variables *)
         |> List.filter (fun (instr : Bril.Instr.t) ->
                match get_instr_dest instr with
                (* of course, keep all instructions that don't have destinations *)
                | None -> true
                | Some (dst_name, _) ->
                    let inst_used = ArgumentSet.mem dst_name used in
                    (* set flag if we transformed the code *)
                    if not inst_used then changed_something := true else ();
                    inst_used)
         |> Bril.Func.set_instrs func)
  |> fun (transformed_prog : Bril.t) ->
  if !changed_something then elim_global_unused_assigns transformed_prog
  else prog

(** [converge_basic_block block] is a basic block that has no instances of
    duplicated assignments to the same variable, up to the algorithm we learned
    in class. *)
let rec converge_basic_block (block : Bril.Instr.t list) : Bril.Instr.t list =
  let changed_something : bool ref = create_flag () in
  block
  (* out of a basic block, produce a set of instructions that are safe to delete *)
  |> List.fold_left
       (fun ((i, map, to_delete) : int * int VariableMap.t * InstructionSet.t)
            (instr : Bril.Instr.t) ->
         (* in this new map, discard all bindings whose keys are used as an argument *)
         (match get_instr_args instr with
         | None -> map
         | One arg -> VariableMap.remove arg map
         | Two (arg1, arg2) ->
             map |> VariableMap.remove arg1 |> VariableMap.remove arg2
         | List args ->
             List.fold_left (fun acc arg -> VariableMap.remove arg acc) map args)
         |> fun map_post_removal ->
         (* delete instruction that is overwritten (if it exists) and update/insert 
          a new value for the key representing the destination variable name *)
         (match get_instr_dest instr with
         | None -> (map_post_removal, to_delete)
         | Some (dst_name, _) ->
             ( VariableMap.add dst_name i map_post_removal,
               match VariableMap.find_opt dst_name map_post_removal with
               | None -> to_delete
               | Some prev_i ->
                   changed_something := true;
                   InstructionSet.add prev_i to_delete ))
         |> fun (map_post_addition, to_delete) ->
         (i + 1, map_post_addition, to_delete))
       (0, VariableMap.empty, InstructionSet.empty)
  |> fun (_, _, to_delete) ->
  block
  (* within the basic block, retain instructions iff they aren't in to_delete *)
  |> List.filteri (fun instr_id _ ->
         to_delete |> InstructionSet.mem instr_id |> not)
  (* re-iterate if you made modifications this time around *)
  |> if !changed_something then converge_basic_block else fun x -> x

(** [elim_locally_killed_assigns] calls [converge_basic_block] on every basic
    block it finds in a bril program. This is the function that implements local
    dead code elimination. *)
let elim_locally_killed_assigns : Bril.t -> Bril.t =
  List.map (fun (func : Bril.Func.t) ->
      func |> Bril.Func.instrs |> Cfg.form_blocks
      |> List.map converge_basic_block
      |> List.concat |> Bril.Func.set_instrs func)
