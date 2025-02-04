module Value = struct
  type t = { op : string; args : int list }
  (** A type to represent the abstract value that an instruction computes *)

  let init (op : string) (args : int list) : t = { op; args }

  (** [( = ) p q] is a boolean that represents structural equality between
      values [p] and [q]. *)
  let ( = ) (v1 : t) (v2 : t) : bool =
    v1.op = v2.op
    &&
    try List.for_all2 Int.equal v1.args v2.args
    with Invalid_argument _ -> false
end

module FunctionVariables = struct
  type t = Int.t
  (** A type to implement the interface to provide fresh variables in this
      function's scope *)

  (** [init f] is an integer representing one greater than the largest
      contiguous integer found in any variable name in the function [f]'s scope.
      This is to guarantee that a call to [fresh] will return a completely new
      variable name. *)
  let init (func : Bril.Func.t) : t =
    func |> Bril.Func.instrs
    |> List.fold_left
         (fun cur_max instr ->
           match Bril.Instr.dest instr with
           | None -> cur_max
           | Some (dst, _) ->
               dst
               |> String.fold_left
                    (fun (instr_cur_max, num_contiguous_digits) char_elt ->
                      (match char_elt with
                      | '0' -> Some 0
                      | '1' -> Some 1
                      | '2' -> Some 2
                      | '3' -> Some 3
                      | '4' -> Some 4
                      | '5' -> Some 5
                      | '6' -> Some 6
                      | '7' -> Some 7
                      | '8' -> Some 9
                      | _ -> None)
                      |> function
                      | None -> (Int.max instr_cur_max num_contiguous_digits, 0)
                      | Some digit ->
                          (instr_cur_max, (num_contiguous_digits * 10) + digit))
                    (0, 0)
               |> fun (a, b) -> Int.max a b |> Int.max cur_max)
         0
    |> ( + ) 1

  (** [fresh t] produces a new variable in the scope of the function used to
      initialize [t] *)
  let fresh (i : t) : string * t = ("v" ^ string_of_int i, Int.succ i)
end

module Table = Map.Make (Int)
module Var2Num = Map.Make (String)

(** [op instr] is a string representation of the "op code" for an instruction *)
let op : Bril.Instr.t -> string =
  let open Bril.Instr in
  function
  | Label _ -> "label"
  | Const _ -> "const"
  | Binary (_, op, _, _) -> Bril.Op.Binary.to_string op
  | Unary (_, op, _) -> Bril.Op.Unary.to_string op
  | Jmp _ -> "jmp"
  | Br _ -> "br"
  | Call _ -> "call"
  | Ret _ -> "ret"
  | Print _ -> "print"
  | Nop -> "nop"
  | Phi _ -> "phi"
  | Speculate -> "spec"
  | Commit -> "commit"
  | Guard _ -> "guard"
  | Alloc _ -> "alloc"
  | Free _ -> "free"
  | Store _ -> "store"
  | Load _ -> "load"
  | PtrAdd _ -> "ptradd"

(** [args instr] is a list of strings representing the arguments of instruction
    [instr]. I had to redefine it because Bril.Instr.args raises an exception if
    you call [args] on an instruction that doesn't have arguments (instead of
    producing an empty list). *)
let args : Bril.Instr.t -> string list =
  let open Bril.Instr in
  let open Bril in
  function
  | Binary (_, _, arg1, arg2) -> [ arg1; arg2 ]
  | Unary (_, _, arg) | Br (arg, _, _) | Guard (arg, _) -> [ arg ]
  | Call (_, _, args) | Print args -> args
  | Alloc ((_ : Dest.t), arg) -> [ arg ]
  | Free arg -> [ arg ]
  | Store (arg1, arg2) -> [ arg1; arg2 ]
  | Load ((_ : Dest.t), arg) -> [ arg ]
  | PtrAdd ((_ : Dest.t), arg1, arg2) -> [ arg1; arg2 ]
  | Ret arg -> ( match arg with None -> [] | Some arg -> [ arg ])
  | Phi ((_ : Dest.t), label_and_args) -> List.map snd label_and_args
  | Nop | Speculate | Commit | Label _ | Const (_, _) | Jmp _ -> []

(** [drop n lst] is a list identical to [lst], but with the first [n] elements
    dropped. *)
let drop n lst =
  lst
  |> List.fold_left
       (fun (i, lst) elt -> if i > n then (i, elt :: lst) else (i + 1, lst))
       (1, [])
  |> snd |> List.rev

(** A function to lazily create pointer to a fresh pointer creator value, so we
    can use a new scope at every Bril function definition. *)
let create_box func = ref (FunctionVariables.init func)

module ArgumentSet = Set.Make (String)
(** A module wrapping a data type and functions to manipulate a set of
    arguments. *)

(** [init_lvn_datastructurs block] is a tuple that represents the LVN data
    structures before LVN takes place on [block]; in particular, it will
    initialize variables defined in another part of the function that may be
    called from this block.*)
let init_lvn_datastructs (block : Bril.Instr.t list) :
    int Var2Num.t * (Value.t option * string) Table.t =
  ArgumentSet.fold
    (fun var (cloud, table, i) ->
      (Var2Num.add var i cloud, Table.add i (None, var) table, i + 1))
    (block |> List.rev
    |> List.fold_left
         (fun acc instr ->
           List.fold_left
             (fun acc' arg -> ArgumentSet.add arg acc')
             (match Bril.Instr.dest instr with
             | None -> acc
             | Some (dest, _) -> ArgumentSet.remove dest acc)
             (args instr))
         ArgumentSet.empty)
    (Var2Num.empty, Table.empty, 0)
  |> fun (x, y, _) -> (x, y)

(** [canonicalize_arguments instr] is a function that either sorts the arguments
    of [instr] into a canonical form (to maximize chances that the value has
    been computed before), or leaves them alone, depending on if the operation
    is commutative or not. *)
let canonicalize_arguments =
  let open Bril.Instr in
  let open Bril.Op in
  function
  | Binary (_, Binary.Add, _, _)
  | Binary (_, Binary.Mul, _, _)
  | Binary (_, Binary.And, _, _)
  | Binary (_, Binary.Or, _, _) ->
      List.sort Int.compare
  | _ -> fun x -> x

(** [perform_lvn_on_block] is a pass over a single block to transform it using
    LVN. It features a gigantic [fold_left] that computes the data structures
    and iteratively builds up the basic block with modified instructions. *)
let perform_lvn_on_block (ptr : FunctionVariables.t ref)
    (block : Bril.Instr.t list) =
  (* initalize lvn data structures to hold out-of-scope variables *)
  let cloud_init, table_init = init_lvn_datastructs block in
  block
  |> List.fold_left
       (* for every instruction, transform according to LVN *)
       (fun (block_instrs, table, cloud, instr_index) instr ->
         match Bril.Instr.dest instr with
         (* For instrs that don't have destinations, simply look through
             arguments and swap out for any canonical variables *)
         | None ->
             instr |> args
             |> List.map (fun arg ->
                    let num = Var2Num.find arg cloud in
                    table |> Table.find num |> snd)
             |> fun arg_lst ->
             Bril.Instr.set_args arg_lst instr |> fun transformed_instr ->
             (transformed_instr :: block_instrs, table, cloud, instr_index + 1)
         (* Here, the destination does exist. Now, we can proceed with LVN. *)
         | Some dest ->
             (* Choose the instructions that should be guaranteed their own values 
             and shouldn't point to any other canonical value (ie. when value_opt = None). These include constants and pointer-producing expressions. 
             For pointers, this is desired behavior because we don't want to Id 
             an existing pointer if subexpressions match; we want to produce a new one. 
             Finally, I simply choose not to store information about constants 
             because I don't do constant folding. *)
             let value_opt : Value.t option =
               match (instr, snd dest) with
               | Bril.Instr.Const _, _
               | Bril.Instr.Phi _, _
               | _, Bril.Bril_type.PtrType _ ->
                   None
               | other, _ ->
                   other |> args
                   |> List.map (fun arg -> Var2Num.find arg cloud)
                   |> canonicalize_arguments instr
                   |> fun lst -> Some (Value.init (op instr) lst)
             in
             (* Find the binding in table that matches value. If value_opt is None,
             you're guaranteed to not find any matches, hence you get your own row. *)
             let canonical_rep_opt =
               Table.fold
                 (fun num (existing_value, var) acc ->
                   match acc with
                   (* if you found it, keep passing it along *)
                   | Some _ -> acc
                   (* if you didn't find it yet, see if the current binding works *)
                   | None -> (
                       match (existing_value, value_opt) with
                       | None, _ | _, None -> None
                       | Some (v1 : Value.t), Some (v2 : Value.t) ->
                           if Value.( = ) v1 v2 then Some (num, var) else None))
                 table None
             in
             (* compute the new block after transforming this instruction,
                the new table after potentially adding a new entry for this instruction's value, 
                and the number used to reference this instruction's value*)
             let block_instrs', table', num =
               match canonical_rep_opt with
               (* A canonical variable for the value exists; replace current
                    instruction with Id operation on canonical variable *)
               | Some (num, canonical_var) ->
                   Bril.Instr.Unary (dest, Bril.Op.Unary.Id, canonical_var)
                   |> fun replaced_instr ->
                   (replaced_instr :: block_instrs, table, num)
               (* Value didn't match anything that exists; add this to the 
                    LVN data structures *)
               | None ->
                   (* fresh num is the number used to represent the never-before-computed value 
                    associated with the current instruction *)
                   let fresh_num =
                     Table.fold (fun _ _ acc -> acc + 1) table 0
                   in
                   (* the destination of the current instruction *)
                   let dest_var =
                     match
                       List.exists
                         (fun future_instr ->
                           match Bril.Instr.dest future_instr with
                           | None -> false
                           | Some (future_dest, _) -> fst dest = future_dest)
                         (drop (instr_index + 1) block)
                     with
                     | true ->
                         (* create fresh var if this instr's natural dest will be overwritten 
                            later in the block. *)
                         let var, updated_val = FunctionVariables.fresh !ptr in
                         ptr := updated_val;
                         var
                         (* otherwise, just use the dest this instr came with *)
                     | false -> fst dest
                   in
                   let table' =
                     Table.add fresh_num (value_opt, dest_var) table
                   in

                   ( (instr
                     (* set arguments to refer to the canonical variables 
                        for their respective values *)
                     |> Bril.Instr.set_args
                          (instr |> args
                          |> List.map (fun arg ->
                                 let num = Var2Num.find arg cloud in
                                 table' |> Table.find num |> snd))
                     (* set the destination of this instruction to its potentially changed name *)
                     |> Bril.Instr.set_dest (Some (dest_var, snd dest)))
                     :: block_instrs,
                     table',
                     fresh_num )
             in
             (* add a map from this variable to the identifiers for the canonical variable *)
             let cloud' = Var2Num.add (fst dest) num cloud in
             (block_instrs', table', cloud', instr_index + 1))
       ([], table_init, cloud_init, 0)
  (* return the basic block in the right order! *)
  |> fun (x, _, _, _) -> List.rev x

(** implements lvn! *)
let lvn : Bril.t -> Bril.t =
  List.map (fun (func : Bril.Func.t) ->
      let fresh_var_type_pointer = create_box func in
      func |> Bril.Func.instrs |> Cfg.form_blocks
      |> List.map (fun (block : Bril.Instr.t list) ->
             block |> perform_lvn_on_block fresh_var_type_pointer)
      |> List.concat |> Bril.Func.set_instrs func)
