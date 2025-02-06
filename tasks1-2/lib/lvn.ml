module Value = struct
  (** A type to represent the abstract value that an instruction computes *)
  type t =
    | Constant of { typ : string; const : string }
    | NonConstant of { op : string; args : int list }

  let init_const (typ : string) (const : string) = Constant { typ; const }

  let init_nonconst (op : string) (args : int list) : t =
    NonConstant { op; args }

  (** [( = ) p q] is a boolean that represents structural equality between
      values [p] and [q]. *)
  let ( = ) (v1 : t) (v2 : t) : bool =
    match (v1, v2) with
    | Constant v1, Constant v2 -> v1.typ = v2.typ && v1.const = v2.const
    | NonConstant v1, NonConstant v2 -> (
        v1.op = v2.op
        &&
        try List.for_all2 Int.equal v1.args v2.args
        with Invalid_argument _ -> false)
    | _ -> false
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
  | Call (_, func_name, _) -> "call " ^ func_name
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

let string_of_const_type (const : Bril.Const.t) : string =
  match const with Int _ -> "int" | Bool _ -> "bool" | Float _ -> "float"

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

(** Given an instruction [instr], replace its arguments with the canonical
    variables holding the same values. *)
let replace_args_with_canonical instr cloud table =
  instr |> args
  |> List.map (fun arg ->
         let num = Var2Num.find arg cloud in
         table |> Table.find num |> snd)
  |> fun arg_lst -> Bril.Instr.set_args arg_lst instr

(** Will produce a fresh variable for the destination iff the destination will
    be overwritten later. This is to guarantee that if a later sub-expression
    matches this one, the value won't be associated to the wrong variable. *)
let get_dest_of_transformed_instr curr_dest block pointer instr_index =
  if
    List.exists
      (fun future_instr ->
        match Bril.Instr.dest future_instr with
        | None -> false
        | Some (future_dest, _) -> fst curr_dest = future_dest)
      (drop (instr_index + 1) block)
  then (
    (* create fresh var if this instr's natural dest will be overwritten 
           later in the block. *)
    let var, updated_val = FunctionVariables.fresh !pointer in
    pointer := updated_val;
    var (* otherwise, just use the dest this instr came with *))
  else fst curr_dest

(** [perform_lvn_on_block] is a pass over a single block to transform it using
    LVN. It features a gigantic [fold_left] that computes the data structures
    and iteratively builds up the basic block with modified instructions. *)
let perform_lvn_on_block (ptr : FunctionVariables.t ref)
    (block : Bril.Instr.t list) : Bril.Instr.t list =
  (* initalize lvn data structures to hold out-of-scope variables *)
  let cloud_init, table_init = init_lvn_datastructs block in
  block
  |> List.fold_left
       (* for every instruction, transform according to LVN *)
       (fun ((block_instrs, table, cloud, instr_index) :
              Bril.Instr.t list
              * (Value.t option * string) Table.t
              * int Var2Num.t
              * int) (instr : Bril.Instr.t) ->
         match Bril.Instr.dest instr with
         (* For instrs that don't have destinations, simply look through
             arguments and swap out for any canonical variables *)
         | None ->
             let transformed = replace_args_with_canonical instr cloud table in
             (transformed :: block_instrs, table, cloud, instr_index + 1)
         (* Here, the destination does exist, meaning we can proceed with LVN. First,
          see if we can let the destination of an Id operation refer to the same value 
          as its argument. *)
         | Some dest -> (
             match
               ( instr,
                 match instr with
                 | Bril.Instr.Unary (_, Bril.Op.Unary.Id, id_arg) ->
                     block
                     |> drop (instr_index + 1)
                     |> List.exists (fun instr ->
                            match Bril.Instr.dest instr with
                            | None -> false
                            | Some (future_dest, _) -> future_dest = id_arg)
                     |> not
                 | _ -> false )
             with
             (* If there don't exist any future instructions where the argument 
                of the current Id is overwritten, just let this destination point
                to the same value as its argument. *)
             | Bril.Instr.Unary (_, Bril.Op.Unary.Id, id_arg), true ->
                 let new_instr : Bril.Instr.t =
                   replace_args_with_canonical instr cloud table
                 in
                 let cloud' =
                   Var2Num.add (fst dest) (Var2Num.find id_arg cloud) cloud
                 in
                 (new_instr :: block_instrs, table, cloud', instr_index + 1)
             (* In this case, either the instruction is not an Id or the argument 
              to the Id will be overwritten (hence making it wrong to have the dest value
              tied to the argument value)*)
             | _ ->
                 (*  Choose the instructions that should be guaranteed their own values 
                 and shouldn't point to any other canonical value (this happens when 
                 value_opt = None). These include pointer-producing expressions and function calls. 
                 For pointers, this is desired behavior because we don't want to Id 
                 an existing pointer if subexpressions match; we want to produce a new one. *)
                 let value_opt : Value.t option =
                   match (instr, snd dest) with
                   | Bril.Instr.Phi _, _
                   (* Retain calls to functions to maintain possible side effects *)
                   | Bril.Instr.Call _, _
                   (* Retain pointer-producing expressions for side-effect reasons *)
                   | _, Bril.Bril_type.PtrType _ ->
                       None
                   | Bril.Instr.Const (_, const), _ ->
                       Some
                         (Value.init_const
                            (string_of_const_type const)
                            (Bril.Const.to_string const))
                   | other, _ ->
                       other |> args
                       |> List.map (fun arg -> Var2Num.find arg cloud)
                       |> canonicalize_arguments instr
                       |> fun lst -> Some (Value.init_nonconst (op instr) lst)
                 in
                 (* Find the binding in table that matches value. If value_opt is None,
                you're guaranteed to not find any matches, hence you get your own row. *)
                 let canonical_rep_opt : (int * string) option =
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
                               if Value.( = ) v1 v2 then Some (num, var)
                               else None))
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
                   (* Subexpr. didn't match anything that exists; add this to the 
                    LVN data structures *)
                   | None ->
                       ( get_dest_of_transformed_instr dest block ptr instr_index,
                         Table.fold (fun _ _ acc -> acc + 1) table 0 )
                       |> fun (trans_dest, fresh_num) ->
                       Table.add fresh_num (value_opt, trans_dest) table
                       |> fun table' ->
                       replace_args_with_canonical instr cloud table'
                       |> Bril.Instr.set_dest (Some (trans_dest, snd dest))
                       |> fun new_instr ->
                       (* set the destination of this instruction to its potentially changed name *)
                       (new_instr :: block_instrs, table', fresh_num)
                 in
                 (* add a map from this variable to the identifiers for the canonical variable *)
                 let cloud' = Var2Num.add (fst dest) num cloud in
                 (block_instrs', table', cloud', instr_index + 1)))
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
