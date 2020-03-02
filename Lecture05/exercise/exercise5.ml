open QCheck


(* Exercise 1*)

module H1 =
struct
  type cmd =
    | Add of string * int
    | Remove of string
    | Find of string
    | Mem of string [@@deriving show]
end

module H2 =
struct
  type cmd =
    | Add of string * int
    | Remove of string
    | Find of string
    | Mem of string [@@deriving show { with_path = false }]
end

(* Use the above like this: H1.shows_cmd (H1.Add ("test", 2)) *)

(* End Exercise 1*)


