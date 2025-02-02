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

let lvn : Bril.t -> Bril.t =
  List.map (fun (func : Bril.Func.t) ->
      func |> Bril.Func.instrs |> Cfg.form_blocks
      |> List.map (fun (block : Bril.Instr.t list) ->
             block
             |> List.fold_left
                  (* for every instruction, transform according to LVN *)
                  (fun ((block_instrs, table, cloud) :
                         Bril.Instr.t list
                         * (Value.t option * Bril.Instr.arg) Table.t
                         * int Var2Num.t) (instr : Bril.Instr.t) ->
                    let value : Value.t option =
                      instr |> Bril.Instr.args
                      |> List.map (fun arg -> Var2Num.find arg cloud)
                      |> List.sort Int.compare (* canonicalize arguments *)
                      |> function
                      | [] -> None
                      | lst -> Some (Value.init (op instr) lst)
                    in
                    (* find the binding in table that matches value *)
                    match
                      Table.fold
                        (fun id (existing_value, var) acc ->
                          match acc with
                          | Some _ -> acc
                          | None -> (
                              match (existing_value, value) with
                              | None, _ | _, None -> None
                              | Some v1, Some v2 ->
                                  if Value.( = ) v1 v2 then
                                    Some (id, existing_value, var)
                                  else None))
                        table None
                    with
                    (* if the binding already exists *)
                    | Some _ -> (block_instrs, table, cloud)
                    (* if the binding doesn't exist yet *)
                    | None -> (block_instrs, table, cloud))
                  ([], Table.empty, Var2Num.empty)
             |> fun (x, _, _) -> List.rev x)
      |> List.concat |> Bril.Func.set_instrs func)
