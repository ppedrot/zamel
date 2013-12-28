(* Copyright 2007 Pierre-Marie Pédrot *)
(*
    This file is part of zamel.

    zamel is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    zamel is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Ulexing
open Converter
open Converter_parser

let regexp space = [' ''\t']
let regexp char = [^'\n']
let regexp eol = ('\n' | eof)

let regexp ident = ['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z''_']*

let regexp sec_open = "[" space*
let regexp sec_clos = space* "]"
let regexp to_ipa = sec_open "Script to IPA" sec_clos
let regexp to_script = sec_open "IPA to Script" sec_clos
let regexp reserved = sec_open "Reserved" sec_clos

let regexp equal = '='
let regexp comment = '#'
let regexp letter = [^'=''\\''\n']
let regexp sequence =  (letter | "\\\\" | "\\=" )+

let is_escaped c = c = 92

let unescape s =
  let fold (len, is_esc) c =
    if is_escaped c && not is_esc then
      (len, true)
    else
      (succ len, false)
  in
  let l, _ = Array.fold_left fold (0, false) s in
  let m = Array.length s in
  let ans = Array.make l 0 in
  let i = ref 0 in
  let j = ref 0 in
  while !i < m do
    let c = s.(!i) in
    if is_escaped c then
    (
      ans.(!j) <- s.(!i + 1);
      incr i
    )
    else
      ans.(!j) <- c;
    incr i;
    incr j;
  done;
  ans

let get_token = function
| "l_paren" -> LPAREN
| "r_paren" -> RPAREN
| "l_brace" -> LBRACE
| "r_brace" -> RBRACE
| "l_brack" -> LBRACK
| "r_brack" -> RBRACK
| "l_chevr" -> LCHEVR
| "r_chevr" -> RCHEVR
| "star" -> STAR
| "sep" -> SEP
| "holder" -> HOLDER
| "anchor" -> ANCHOR
| "comment" -> COMMENT
| _ -> raise Ulexing.Error

let position line =
{
  Lexing.pos_fname = "dummy";
  Lexing.pos_lnum = line;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let main name lexbuf =

  let mode = ref `Default in
  let q = Queue.create () in
  let line = ref 1 in
  let push x =
    if x = NEWLINE then incr line;
    Queue.push x q
  in
  push (NAME name);

  let lex_default dummy_lexbuf = lexer
  | eof -> push NEWLINE; push EOF
  | space+ -> ()
  | comment char* eol -> push NEWLINE
  | eol -> push NEWLINE
  | to_ipa space* eol ->
    mode := `Dict;
    push TO_IPA;
    push NEWLINE
  | to_script space* eol ->
    mode := `Dict;
    push TO_SCRIPT;
    push NEWLINE
  | reserved space* eol ->
    mode := `Reserved;
    push RESERVED;
    push NEWLINE
  in

  let lex_dict dummy_lexbuf = lexer
  | eof -> mode := `Default
  | space+ -> ()
  | comment char* eol -> push NEWLINE
  | eol -> push NEWLINE
  | (to_ipa | to_script | reserved) space* eol ->
    rollback lexbuf;
    mode := `Default
  | sequence equal sequence eol ->
    rollback lexbuf;
    let seq = (lexer sequence -> unescape (lexeme lexbuf)) lexbuf
    and _ = (lexer equal -> ()) lexbuf
    and img = (lexer sequence -> unescape (lexeme lexbuf)) lexbuf
    in
    push (SEQUENCE seq);
    push EQUAL;
    push (SEQUENCE img)
  in

  let lex_reserved dummy_lexbuf = lexer
  | eof -> mode := `Default
  | space+ -> ()
  | comment char* eol -> push NEWLINE
  | eol -> push NEWLINE
  | (to_ipa | to_script | reserved) space* eol ->
    rollback lexbuf;
    mode := `Default
  | ident space* equal sequence eol ->
    let id = (lexer ident -> utf8_lexeme lexbuf) lexbuf
    and _ = (lexer space* equal -> ()) lexbuf
    and img = (lexer sequence -> lexeme lexbuf) lexbuf
    in
    push (get_token id);
    push EQUAL;
    push (SEQUENCE img)
  in

  function dummy_lexbuf ->
    while Queue.is_empty q do
    (
      match !mode with
      | `Default -> lex_default
      | `Dict -> lex_dict
      | `Reserved -> lex_reserved
    ) dummy_lexbuf lexbuf
    done;
    dummy_lexbuf.Lexing.lex_curr_p <- position !line;
    Queue.pop q