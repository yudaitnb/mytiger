type unique = unit ref [@@deriving show, eq]

type ty =
  | UNIT
  | NIL
  | INT
  | STRING
  | RECORD of (Symbol.symbol * ty) list * unique
  | ARRAY of ty * unique
  | FUNCTION of ty list * ty
  | NAME of Symbol.symbol * ty option ref
[@@deriving show, eq]

let rec actual_ty = function
  | NAME (_,{contents=Some t}) -> actual_ty t
  | t                          -> t