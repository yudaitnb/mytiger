open Printf
open Location

(* string値を受け取って出力する *)
let debug (str : string) (loc : location) = 
  let (startp, endp) = loc in
  let lnum = startp.pos_lnum in
  let start_pos = startp.pos_cnum - startp.pos_bol + 1 in
  let end_pos_sub = endp.pos_cnum - endp.pos_bol + 1 in
  let chars = end_pos_sub - start_pos in
  let end_pos = 
    if chars = 0
      then end_pos_sub
      else end_pos_sub - 1
  in
  let posinfo =
    if chars = 1
      then Format.sprintf "line %d, character %d: " lnum start_pos
      else Format.sprintf "line %d, characters %d-%d: " lnum start_pos end_pos
  in
  printf "%s\n" (posinfo ^ str)

let printl (str : string) = printf "%s\n" str

let printllst (strlst : string list) =
  let flatten = List.fold_left
    (fun acc s -> acc ^ " " ^ s)
    ("")
    (strlst)
  in
    printl flatten