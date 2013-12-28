/* Copyright 2007 Pierre-Marie Pédrot */
/*
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
*/

%{
open Converter
%}

%token NEWLINE EOF EQUAL
%token TO_IPA TO_SCRIPT RESERVED
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE LCHEVR RCHEVR SEP STAR HOLDER ANCHOR COMMENT
%token <int array> SEQUENCE
%token <string> NAME

%start parse
%type <Converter.script> parse

%%

/* Lines */

newline:
| NEWLINE { }
| newline NEWLINE { $1 }
;

/* Sequences */

dict_lines:
| dict_lines SEQUENCE EQUAL SEQUENCE newline { add $1 $2 $4 }
| { create_dict () }
;

to_ipa:
| TO_IPA newline dict_lines { `To_ipa $3 }
;

to_script:
| TO_SCRIPT newline dict_lines { `To_script $3 }
;

/* Reserved */

reserved_token:
| LPAREN { 0 }
| RPAREN { 1 }
| LBRACE { 2 }
| RBRACE { 3 }
| LBRACK { 4 }
| RBRACK { 5 }
| LCHEVR { 6 }
| RCHEVR { 7 }
| STAR { 8 }
| SEP { 9 }
| HOLDER { 10 }
| ANCHOR { 11 }
| COMMENT { 12 }
;

reserved_lines:
| reserved_lines reserved_token EQUAL SEQUENCE newline { ($2, $4) :: $1 }
| { [] }
;

reserved:
| RESERVED newline reserved_lines { `Reserved $3 }
;

/* Main */

name:
| NAME newline { $1 }
| NAME { $1 }
;

sections:
| sections to_ipa { $2 :: $1 }
| sections to_script { $2 :: $1 }
| sections reserved { $2 :: $1 }
| { [] }
;

parse:
| name sections EOF
  {
    let array = [|
      to_int_array "(";
      to_int_array ")";
      to_int_array "{";
      to_int_array "}";
      to_int_array "[";
      to_int_array "]";
      to_int_array "<";
      to_int_array ">";
      to_int_array "*";
      to_int_array "/";
      to_int_array "_";
      to_int_array "#";
      to_int_array "#";
    |] in
    let to_ipa_dict =
      match List.find_all (function `To_ipa _ -> true | _ -> false) $2 with
      | [] -> create_dict ()
      | [`To_ipa d] -> d
      | _ -> raise Parsing.Parse_error
    and to_script_dict =
      match List.find_all (function `To_script _ -> true | _ -> false) $2 with
      | [] -> create_dict ()
      | [`To_script d] -> d
      | _ -> raise Parsing.Parse_error
    and reserved =
      match List.find_all (function `Reserved _ -> true | _ -> false) $2 with
      | [] -> []
      | [`Reserved r] -> r
      | _ -> raise Parsing.Parse_error
    in
    let iter (token, seq) = array.(token) <- seq in
    List.iter iter reserved;
    create_script $1 array to_ipa_dict to_script_dict
  }
;

%%
