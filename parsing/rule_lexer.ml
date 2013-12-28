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
open Ipadata
open Converter
open Rule_parser

exception Misformed_line
exception Unknown_section of string

let regexp space = [' ''\t']
let regexp char = [^'\n']
let regexp eol = ('\n' | eof)
let regexp ident = ['a'-'z''A'-'Z']['0'-'9''a'-'z''A'-'Z''_']*
let regexp digit = ['0'-'9']
let regexp equal = '='
let regexp plus = '+'
let regexp minus = ['-'"−"]
let regexp homorganic = '~'
let regexp coma = ','

let get_reserved converter =
  let fold accu ((l, r), (lt, rt)) =
    (l, lt) :: (r, rt) :: accu
  in
  List.fold_left fold
  [
    [|10|], NEWLINE;
    [|-1|], NEWLINE;
    converter#star, STAR;
    converter#sep, SEP;
    converter#holder, HOLDER;
    converter#anchor, ANCHOR;
  ]
  [
    converter#paren, (LPAREN, RPAREN);
    converter#brace, (LBRACE, RBRACE);
    converter#brack, (LBRACK, RBRACK);
    converter#chevr, (LCHEVR, RCHEVR)
  ]

let get_id l =
  let ans = (lexer ident -> utf8_lexeme lexbuf) l in
  start l;
  ans

let get_number l =
  let ans = (lexer ('-')? digit+ -> int_of_string (utf8_lexeme lexbuf)) l in
  start l;
  ans

let get_digit l =
  let ans = (lexer digit -> int_of_string (utf8_lexeme lexbuf)) l in
  start l;
  ans

let ignore_spaces l =
  let _ = (lexer space* -> ()) l in
  start l

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

let discard n lexbuf =
for i = 0 to n - 1 do
  ignore (next lexbuf);
done;
start lexbuf

let position line =
{
  Lexing.pos_fname = "dummy";
  Lexing.pos_lnum = line;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = 0;
}


let main converter lexbuf =

  let q = Queue.create () in
  let line = ref 1 in
  let push x =
    if x = NEWLINE then incr line;
    Queue.push x q
  in
  let mode = ref `Default in

  let rec verbatim = lexer
  | eof -> ()
  | space+ ->
    verbatim lexbuf
  | plus ->
    push PLUS;
    verbatim lexbuf
  | minus ->
    push MINUS;
    verbatim lexbuf
  | homorganic ->
    push VAR;
    verbatim lexbuf
  | ident ->
    push (IDENT (utf8_lexeme lexbuf));
    verbatim lexbuf
  | coma ->
    push SEP;
    verbatim lexbuf
  | _ ->
    rollback lexbuf
  in

(*
La sekva funkcio estas plena aĉaĵo: la kalkulado estas ege malbona.
Tamen ĝi funkcias... Reparinda!
*)

  let lex_rule lexbuf =
    rollback lexbuf;
    let reserved = get_reserved converter in
    let i = ref (get_start lexbuf)
    and j = ref 0
    and part = ref 0
    and group_nb = ref 0 in
    let apply = function
    | SEP ->
      incr part;
      if !part = 3 then verbatim lexbuf
    | LBRACE ->
      if !part = 0 then
      (
        incr group_nb;
        push (NUMBER !group_nb)
      );
      if !part = 1 then
      (
        let _ = ignore_spaces lexbuf
        and numero = get_digit lexbuf in
        push (NUMBER numero)
      );
(*       if !part = 2 then verbatim lexbuf; *)
    | LBRACK -> verbatim lexbuf
    | LCHEVR -> verbatim lexbuf
    | _ -> ()
    in
    let rec find_reserved = function
    | (symbol, token) :: q ->
      if begin_with symbol lexbuf then
      (
        let phrase = Array.sub (get_buf lexbuf) !i !j in
        let iter c = match get_IPA_type c with
        | Base_char | Suprasegmental_char -> push (CHAR c)
        | Space_char | Diacritic_char -> push (MODIFIER c)
        | Not_IPA_char -> raise (Unknown_char c)
        in
        Array.iter iter (converter#to_ipa phrase);
        discard (Array.length symbol) lexbuf;
        push token;
        apply token;
        i := get_start lexbuf;
        j := 0;
        if token = NEWLINE then () else find_reserved reserved
      )
      else
        find_reserved q
    | [] ->
      incr j;
      discard 1 lexbuf;
      find_reserved reserved
    in
  find_reserved reserved
  in

  let lex_rules dummy_lexbuf = lexer
  | eof -> mode := `Default
  | space+ -> ()
  | eol -> push NEWLINE
  | "Section" space+ ident space* eol ->
    rollback lexbuf;
    mode := `Default
  | "Group" space+ ident space* eol ->
    rollback lexbuf;
    let _ = get_id lexbuf
    and _ = ignore_spaces lexbuf
    and id = get_id lexbuf
    in
    push (GROUP_SUBSECT id)
  | '-'? digit+ space* eol ->
    rollback lexbuf;
    let date = get_number lexbuf in
    push (NUMBER date)
  | ident space* eol ->
    rollback lexbuf;
    let group = get_id lexbuf in
    push (IDENT group)
  | char+ ->
    if begin_with (converter#comment) lexbuf then ()
    else lex_rule lexbuf
  in

  let lex_dialects dummy_lexbuf = lexer
  | eof -> mode := `Default
  | space+ -> ()
  | eol -> push NEWLINE
  | "Section" space+ ident space* eol ->
    rollback lexbuf;
    mode := `Default
  | ident space* equal space* ident space* eol ->
    rollback lexbuf;
    let id1 = get_id lexbuf
    and _ = ignore_spaces lexbuf
    and _ = discard 1 lexbuf
    and _ = ignore_spaces lexbuf
    and id2 = get_id lexbuf
    in
    push (IDENT id1);
    push EQUAL;
    push (IDENT id2)
  | char+ ->
    if begin_with (converter#comment) lexbuf then ()
    else raise Ulexing.Error
  in

  let lex_default dummy_lexbuf = lexer
  | eof -> push NEWLINE; push EOF
  | space+ -> ()
  | eol -> push NEWLINE
  | "Section" space+ ident space* eol ->
    let section_ident =
      rollback lexbuf;
      let _ = (lexer "Section" space+ -> ()) lexbuf
      and ident = get_id lexbuf
      and _ = ignore_spaces lexbuf
      in ident
    in (
    match section_ident with
    | "Rules" ->
      push RULE_SECT;
      mode := `Rules
    | "Groups" ->
      push GROUP_SECT;
      mode := `Rules
    | "Persistent" ->
      push PERSISTENT_SECT;
      mode := `Rules
    | "Dialects" ->
      push DIALECT_SECT;
      mode := `Dialects
    | _ -> raise (Unknown_section section_ident)
    )
  | char+ ->
    if begin_with (converter#comment) lexbuf then ()
    else
    (
      rollback lexbuf;
      push RULE_SECT;
      push NEWLINE;
      mode := `Rules
    )
  in

  function dummy_lexbuf ->
    while Queue.is_empty q do
    (
      match !mode with
      | `Default -> lex_default
      | `Rules -> lex_rules
      | `Dialects -> lex_dialects
    ) dummy_lexbuf lexbuf
    done;
    dummy_lexbuf.Lexing.lex_curr_p <- position !line;
    Queue.pop q