
(* a datatype of arithmetic expressions *)
type aexp =
  | X
  | Lit of int
  | Plus of aexp * aexp
  | Times of aexp * aexp

let mytree = Plus (Lit 1, Times (X, Lit 3))

(* our interpreter of arithmetic expressions *)
let rec interpret xval ae = match ae with
  | X -> xval
  | Lit i -> i
  | Plus (ae0, ae1) ->
    let v0 = interpret xval ae0 in
    let v1 = interpret xval ae1 in
    v0 + v1
  | Times (ae0, ae1) ->
    let v0 = interpret xval ae0 in
    let v1 = interpret xval ae1 in
    v0 * v1

(*  interpret mytree;;  *)

let rec exp_to_string ae = match ae with
  | X -> "x"
  | Lit i -> string_of_int i
  | Plus (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "(" ^ s0 ^ "+" ^ s1 ^ ")"
  | Times (ae0, ae1) ->
    let s0 = exp_to_string ae0 in
    let s1 = exp_to_string ae1 in
    "(" ^ s0 ^ "*" ^ s1 ^ ")"

(*  exp_to_string mytree;;  *)

let rec size ae = match ae with
    | X -> 1
    | Lit i -> 1
    | Plus (ae0, ae1) ->
        1 + size(ae0) + size(ae1)
    | Times (ae0, ae1) -> 
        1 + size(ae0) + size(ae1)



(* a datatype of abstract machine instructions *)
type inst =
  | Load
  | Push of int
  | Add
  | Mult

(* our compiler from arithmetic expressions to instructions *)
let rec compile ae = match ae with
  | X -> [Load]
  | Lit i -> [Push i]
  | Plus (ae0, ae1) ->
    let is0 = compile ae0 in
    let is1 = compile ae1 in
    is0 @ is1 @ [Add]
  | Times (ae0, ae1) ->
    let is0 = compile ae0 in
    let is1 = compile ae1 in
    is0 @ is1 @ [Mult]

(*  compile mytree;;  *)


exception Darn_no_Stack_Element_Add
exception Darn_no_Stack_Element_Multi

let rec run xreg inst stack = match inst with
    | [] -> stack
    | e::elems -> run xreg elems (match e with 
        | Load -> [xreg] @ stack
        | Push i -> [i] @ stack
        | Add -> (match stack with
            | [] -> []
            | stackElem::stackElems -> (match stackElems with
                | [] -> raise Darn_no_Stack_Element_Add
                | e1::remainder -> [(stackElem + e1)] @ remainder))
        | Mult -> (match stack with
            | [] -> []
            | stackElem::stackElems -> (match stackElems with
                | [] -> raise Darn_no_Stack_Element_Multi
                | e2::remainder -> [(stackElem * e2)] @ remainder))) 
