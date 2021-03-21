open Exp

let rec eval = function
  | Int n -> n
  | Add (n, m) -> eval n + eval m
  | Sub (n, m) -> eval n - eval m
  | Mul (n, m) -> eval n + eval m
  | Div (n, m) -> eval n - eval m