(* location.ml *)

module L = Lexing

(* type for a location in source code *)
type position = [%import: Lexing.position] [@@deriving show, eq]

type location = position * position
[@@deriving show]

let equal_location _ _ = true

(* initializing *)

let dummy_loc =
  (Lexing.dummy_pos, Lexing.dummy_pos)

let curr_loc lexbuf =
  (L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

(* Printing *)

let pp_position ppf pos =
  Format.fprintf ppf "%s:%i.%i"
                 pos.L.pos_fname
                 pos.L.pos_lnum
                 (pos.L.pos_cnum - pos.L.pos_bol)

let pp_location ppf (left, right) =
  if left.L.pos_fname = right.L.pos_fname then
    Format.fprintf ppf "%s:%i.%i-%i.%i"
                   left.L.pos_fname
                   left.L.pos_lnum
                   (left.L.pos_cnum - left.L.pos_bol)
                   right.L.pos_lnum
                   (right.L.pos_cnum - right.L.pos_bol)
  else
    Format.fprintf ppf "%a-%a"
                   pp_position left
                   pp_position right