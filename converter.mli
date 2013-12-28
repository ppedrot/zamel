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

type uchar = int
type ustring = uchar array

class type script =
  object
    method name: string
    method paren: ustring * ustring
    method brace: ustring * ustring
    method brack: ustring * ustring
    method chevr: ustring * ustring
    method star: ustring
    method sep: ustring
    method holder: ustring
    method anchor: ustring
    method comment: ustring
    method to_ipa: ustring -> ustring
    method to_script: ustring -> ustring
  end

exception Unknown_sequence of string

type dict

val to_int_array : string -> ustring
val from_int_array : ustring -> string
val skip_spaces : ustring -> ustring

val create_dict : unit -> dict
val add : dict -> ustring -> ustring -> dict
val convert : dict -> ustring -> ustring

val ipa_script : script
val create_script : string -> ustring array -> dict -> dict -> script
