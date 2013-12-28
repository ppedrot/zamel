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

open Xml

type type_IPA = Base_char | Space_char | Diacritic_char | Suprasegmental_char | Not_IPA_char
(* type nature = Suprasegmental | Phoneme *)

exception Unknown_feature of string
exception Unknown_char of int

let get_xml_data file = parse_file file

let get_char x = Utf8.next x 0
let get_escaped x = int_of_string x

let char_map = Hashtbl.create 0

and base_map = Hashtbl.create 0

and modifier_map = Hashtbl.create 0
and rev_modifier_map = Hashtbl.create 0

and feature_map = Hashtbl.create 0
and rev_feature_map = Hashtbl.create 0
and value_map = Hashtbl.create 0
and rev_value_map = Hashtbl.create 0

and p_length = ref 0
and s_length = ref 0

and base_chars = ref []
and supra_chars = ref []

let phoneme_length () = !p_length
let suprasegmental_length () = !s_length

let get_base_chars () = !base_chars
let get_suprasegmental_chars () = !supra_chars

let int_to_phoneme_value feature value = Hashtbl.find rev_value_map (false, feature, value)
let int_to_phoneme_feature feature = Hashtbl.find rev_feature_map (false, feature)

let int_to_suprasegmental_value feature value = Hashtbl.find rev_value_map (true, feature, value)
let int_to_suprasegmental_feature feature = Hashtbl.find rev_feature_map (true, feature)

let phoneme_value_to_int value_name =
  try Hashtbl.find value_map (false, value_name)
  with Not_found -> raise (Unknown_feature value_name)

let phoneme_feature_to_int feature_name =
  try Hashtbl.find feature_map (false, feature_name)
  with Not_found -> raise (Unknown_feature feature_name)

let suprasegmental_value_to_int value_name =
  try Hashtbl.find value_map (true, value_name)
  with Not_found -> raise (Unknown_feature value_name)

let suprasegmental_feature_to_int feature_name =
  try Hashtbl.find feature_map (true, feature_name)
  with Not_found -> raise (Unknown_feature feature_name)

let get_IPA_type char =
  try Hashtbl.find char_map char
  with Not_found -> Not_IPA_char

let from_modifier char = Hashtbl.find modifier_map char
let to_modifier feature value = Hashtbl.find rev_modifier_map (feature, value)

let get_phoneme_value char feature = try Hashtbl.find base_map (false, char, feature) with Not_found -> 0
let get_suprasegmental_value char feature = try Hashtbl.find base_map (true, char, feature) with Not_found -> 0

let add style feature value char =
  Hashtbl.replace char_map char style;
  match style with
  | Base_char | Suprasegmental_char ->
    Hashtbl.replace base_map (style = Suprasegmental_char, char, feature) value
  | Space_char | Diacritic_char ->
    Hashtbl.replace modifier_map char (feature, value);
    Hashtbl.replace rev_modifier_map (feature, value) char
  | Not_IPA_char ->
    failwith "fold"

let iter_element is_suprasegmental feature value accu = function
| Element (style, _, [PCData data]) ->
  if is_suprasegmental then
    begin match style with
    | "supra" ->
      let data = get_char data in
      add Suprasegmental_char feature value data; data :: accu
    | _ -> failwith "fold"
    end
  else
    begin match style with
    | "base" ->
      let data = get_char data in
      add Base_char feature value data;
      data :: accu
    | "space" ->
      let data = get_char data in
      add Space_char feature value data;
      accu
    | "diacritic" ->
      let data = get_escaped data in
      add Diacritic_char feature value data;
      accu
    | _ -> failwith "fold"
    end
| _ -> failwith "fold"

let iter_value is_suprasegmental feature value = function
| Element ("value", ["id", id], data) ->
  Hashtbl.replace rev_value_map (is_suprasegmental, feature, value) id;
  Hashtbl.replace value_map (is_suprasegmental, id) (feature, value);
  let _ = List.fold_left (iter_element is_suprasegmental feature value) [] data in
  value + 1
| Element ("default", ["modifier", modifier], data) ->
  let char = get_escaped modifier in
  Hashtbl.replace char_map char Diacritic_char;
  Hashtbl.replace modifier_map char (feature, 0);
  Hashtbl.replace rev_modifier_map (feature, 0) char;
  value
| _ ->
  failwith "fold"

let iter_feature is_suprasegmental feature = function
| Element ("feature", ["id", id], data) ->
  let _ = List.fold_left (iter_value is_suprasegmental feature) 1 data in
  Hashtbl.replace rev_feature_map (is_suprasegmental, feature) id;
  Hashtbl.replace feature_map (is_suprasegmental, id) feature;
  feature + 1
| _ -> failwith "fold"

let init file =
  Hashtbl.clear char_map;
  Hashtbl.clear base_map;
  Hashtbl.clear modifier_map;
  Hashtbl.clear rev_modifier_map;
  Hashtbl.clear feature_map;
  Hashtbl.clear rev_feature_map;
  Hashtbl.clear value_map;
  Hashtbl.clear rev_value_map;
  let xml_data = parse_file file in
  let iter = function
  | Element ("phoneme_list", _, data) ->
    p_length := List.fold_left (iter_feature false) 0 data
  | Element ("suprasegmental_list", _, data) ->
    s_length := List.fold_left (iter_feature true) 0 data
  | _ ->
    failwith "fold"
  in
  let iter2 =function
  | [Element ("data", _, data)] ->
    List.iter iter data
  | _ ->
    failwith "fold"
  in
iter2 [xml_data];
base_chars := Hashtbl.fold (fun char style accu -> if style = Base_char then char :: accu else accu) char_map [];
supra_chars := Hashtbl.fold (fun char style accu -> if style = Suprasegmental_char then char :: accu else accu) char_map [];
()
