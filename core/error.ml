(*
 * Lexing bufferを受け取り、状況に応じたエラーメッセージを出力する
 * buf : Lexing.lexbuf ocamllexから生成されるlexing buffer
 * proc : string       エラーを起こした処理の名称 (Lexer, Parser)
 * error : string      エラー内容
 *)
let gen_error_message (buf : Lexing.lexbuf) (proc : string) (error : string) =
  let pos = buf.lex_curr_p in
  let lnum = pos.pos_lnum in
  let start = buf.lex_start_pos in
  let curr = buf.lex_curr_pos in
  let last_char =
    (* 仮の条件式（lexbuf.lex_eof_reachedが機能しない） *)
    if (start == curr) then
      "eof"
    else (Lexing.lexeme buf)
  in
  let message = Printf.sprintf
    "line %d, characters %d-%d '%s': %s - %s"
    (lnum)          (* 処理に失敗した行番号 *)
    (start)         (* 処理に失敗した文字の開始位置 *)
    (curr)          (* 処理に失敗した文字位置 *)
    (last_char)     (* 処理に失敗した文字 *)
    (proc)          (* 処理に失敗した処理名 *)
    (error)         (* 処理の失敗の理由 *)
  in
    message