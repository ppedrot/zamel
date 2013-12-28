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

exception Unknown_char of int
exception Unknown_feature of string

type type_IPA =
    Base_char
  | Space_char
  | Diacritic_char
  | Suprasegmental_char
  | Not_IPA_char

val phoneme_length : unit -> int
val suprasegmental_length : unit -> int

val int_to_phoneme_value : int -> int -> string
val int_to_phoneme_feature : int -> string

val phoneme_value_to_int : string -> int * int
val phoneme_feature_to_int : string -> int

val int_to_suprasegmental_value : int -> int -> string
val int_to_suprasegmental_feature : int -> string

val suprasegmental_value_to_int : string -> int * int
val suprasegmental_feature_to_int : string -> int

val get_IPA_type : int -> type_IPA

val from_modifier : int -> int * int
val to_modifier : int -> int -> int

val get_phoneme_value : int -> int -> int
val get_suprasegmental_value : int -> int -> int

val get_base_chars : unit -> int list
val get_suprasegmental_chars : unit -> int list

val init : string -> unit
