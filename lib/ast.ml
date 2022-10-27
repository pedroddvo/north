type expr =
  | Nil
  | Int of int
  | Symbol of string
  | Boolean of bool

  | Intrinsic of (expr Stack.t -> expr)
  | Function of expr list * expr list

  | FunctionGroup of string * expr list

  | AnonId of int
  | AnonFunc of expr list
  | ExprRef of expr
  | Grouping of expr list

  | Def of string * expr list * expr list
  | If of expr list * expr list * expr list
  [@@deriving show]