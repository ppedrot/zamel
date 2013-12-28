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

open Binary
open Ipadata
open Ulexing
open Word_parser

let regexp space = [' ''\t']
let regexp char = [^'\n']
let regexp eol = ('\n' | eof)

let position line =
{
  Lexing.pos_fname = "dummy";
  Lexing.pos_lnum = line;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}

let begin_with pattern lexbuf =
  let l = Array.length pattern in
  let i = ref 0 in
  let t = ref true in
  let seq = get_buf lexbuf in
  let get () =
    try seq.(!i + (get_start lexbuf))
    with _ -> (-1)
  in
  while !t & (!i < l) do
    t := (pattern.(!i) = get ());
    incr i;
  done;
!t & (!i = l)

let main converter lexbuf =

  let q = Queue.create () in
  let line = ref 1 in
  let push x =
    if x = NEWLINE then incr line;
    Queue.push x q
  in

  let lex_line dummy_lexbuf = lexer
  | eof -> push NEWLINE; push EOF
  | space+ -> ()
  | eol -> push NEWLINE
  | char+ ->
    if not (begin_with (converter#comment) lexbuf) then
    (
      let word = converter#to_ipa (lexeme lexbuf) in
      let iter c = match get_IPA_type c with
      | Base_char | Suprasegmental_char -> push (CHAR c)
      | Space_char | Diacritic_char -> push (MODIFIER c)
      | Not_IPA_char -> raise (Unknown_char c)
      in
      Array.iter iter word
    )
  in

  function dummy_lexbuf ->
    while Queue.is_empty q do
      lex_line dummy_lexbuf lexbuf
    done;
    dummy_lexbuf.Lexing.lex_curr_p <- position !line;
    Queue.pop q