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

open Ipadata

exception Misplaced_modifier

type nature = Phoneme | Suprasegmental

type sign =
| Plus
| Minus
| Variable

type value = int

type extended_value =
| Value of (bool * value)
| Homorganic

type elt = int * value

type extended_elt = int * extended_value

type t = Entity of (nature * value array)

type extended_t =
| Ext_Entity of (nature * value array)
| Sparse_Entity of (nature * extended_elt list)

let extend (Entity t) = Ext_Entity t

let unextend = function
| Ext_Entity t -> Entity t
| _ -> invalid_arg "unextend"

let char_cache = Hashtbl.create 0
let reverse_cache = Hashtbl.create 0

let from_char char =
  try Hashtbl.find char_cache char
  with Not_found ->
    let map, map_length, nature = match get_IPA_type char with
    | Suprasegmental_char ->
      get_suprasegmental_value,
      suprasegmental_length (),
      Suprasegmental
    | Base_char ->
      get_phoneme_value,
      phoneme_length (),
      Phoneme
    | _ ->
      invalid_arg "from_char" in
    let ans = Array.init map_length (map char) in
    let binary_t = Entity (nature, ans) in
    Hashtbl.add char_cache char binary_t;
    Hashtbl.add reverse_cache binary_t [|char|];
    binary_t

let from_modifier = from_modifier

let create_generic aux1 aux2 (sign, name) = match sign with
| Plus | Minus ->
  let (feature, value) = aux1 name in
  (feature, Value (sign = Plus, value))
| Variable ->
  let feature = aux2 name in
  (feature, Homorganic)

let from_phoneme_list flist =
  let map = create_generic
    phoneme_value_to_int phoneme_feature_to_int
  in
  Sparse_Entity (Phoneme, List.rev_map map flist)

let from_suprasegmental_list flist =
  let map = create_generic
    suprasegmental_value_to_int suprasegmental_feature_to_int
  in
  Sparse_Entity (Suprasegmental, List.rev_map map flist)

let modify base_t = function
  | [] -> base_t
  | l ->
    let Entity (nature, base_array) = base_t in
    if nature <> Phoneme then raise Misplaced_modifier;
    let new_array = Array.copy base_array in
    let iter (modifier, value) = new_array.(modifier) <- value in
    List.iter iter l;
    Entity (Phoneme, new_array)

let potential_from_binary binary_t =
  let data, data_length, binary = match binary_t with
  | Entity (Phoneme, array) ->
    get_base_chars (),
    phoneme_length (),
    array
  | Entity (Suprasegmental, array) ->
    get_suprasegmental_chars (),
    suprasegmental_length (),
    array
  in
  let ans = List.rev_map (fun char -> char, from_char char) data in
  let distance t1 t2 =
    let Entity (_, a1) = t1
    and Entity (_, a2) = t2 in
    let diff = ref 0 in
    for feature = 0 to data_length - 1 do
      if a1.(feature) <> a2.(feature) then incr diff
    done;
    !diff
  in
  let cmp (char1, ord1) (char2, ord2) =
    (distance ord1 binary_t) - (distance ord2 binary_t)
  in
  let sorted_list = List.sort cmp ans in
  sorted_list

let represent_modifier (opening, closing) represent modifiers =
  let rec aux accu = function
  | [] -> opening :: accu
  | (v, s) :: q ->
    let (sign, converted) = represent v s in
    let value = Converter.to_int_array converted in
    aux (sign :: value :: accu) q
  in
Array.concat (aux [closing] modifiers)

let to_char binary_t =
  try Hashtbl.find reverse_cache binary_t
  with Not_found ->
    let Entity (nature, array) = binary_t in
    let potentials = potential_from_binary binary_t in
    let find_modifiers = fun (Entity (_, farray)) ->
      let ans = ref [] in
      for i = 0 to pred (Array.length farray) do
        if farray.(i) <> array.(i) then ans := (i, array.(i)) :: !ans
      done;
      let map (i, v) = to_modifier i v in
      List.rev_map map !ans
    in
    let rec find = function
    | [] ->
      failwith "represent_binary"
    | (char, goal_binary) :: q ->
      if goal_binary = binary_t then
      (
        Hashtbl.add reverse_cache binary_t [|char|];
        [|char|]
      )
      else if nature = Phoneme then
      (
        try
        (
          let l = find_modifiers goal_binary in
          let tmp = ref l in
          let len = List.length l in
          let ans = Array.create (succ len) char in
          for i = 1 to len do
            ans.(i) <- List.hd !tmp;
            tmp := List.tl !tmp
          done;
          Hashtbl.add reverse_cache binary_t ans;
          ans
        )
        with Not_found -> find q
      )
      else find q
    in
    find potentials

let represent_tuple get_value get_feature i = function
| Value (b, v) ->
  (Converter.to_int_array (if b then "+" else "-"), get_value i v)
| Homorganic ->
  (Converter.to_int_array "~", get_feature i)

let represent_binary_list converter blist =
  converter#to_script (Array.concat (List.map to_char blist))

let represent_extended_list converter blist =
  let rec represent_aux accu_base accu = function
  | [] ->
    let converted =
      represent_binary_list converter (List.rev accu_base)
    in
    Array.concat (List.rev (converted :: accu))
  | Ext_Entity t :: q ->
    represent_aux (Entity t :: accu_base) accu q
  | Sparse_Entity (nature, list) :: q ->
    let converted =
      represent_binary_list converter (List.rev accu_base)
    in
    let tuple, to_value, to_feature = match nature with
    | Phoneme ->
      converter#brack,
      int_to_phoneme_value,
      int_to_phoneme_feature
    | Suprasegmental ->
      converter#chevr,
      int_to_suprasegmental_value,
      int_to_suprasegmental_feature
    in
    let represent_list = represent_tuple to_value to_feature in
    let ext = represent_modifier tuple represent_list list in
    represent_aux [] (ext :: converted :: accu) q
  in
  represent_aux [] [] blist


let match_against binary ext_binary =
  let rec test array = function
  | [] -> true
  | (i, Homorganic) :: q -> test array q
  | (i, Value (b, v)) :: q ->
    ((b && v = array.(i)) || (not b && v <> array.(i))) && test array q
  in
  match binary, ext_binary with
  | Entity t1, Ext_Entity t2 ->
    t1 = t2
  | Entity (n, a), Sparse_Entity (m, l) ->
    (n = m) && (test a l)

let get_value binary i (Entity (n, array)) = array.(i)

let apply binary (ext_binary, homorganic) =
  let handle_sparse_list (nature, list) =
    let map = function
    | (i, Value (b, v)) -> (i, b, v)
    | (i, Homorganic) ->
        begin match homorganic with
        | Some (Entity (n, a)) ->
          if n = nature then
            (i, true, a.(i))
          else
            failwith "apply"
        | None ->
          failwith "apply"
        end
    in
    let li = List.map map list in
    let Entity (n, array) = binary in
    if n <> nature then failwith "apply";
    let new_array = Array.copy array in
    let iter (modifier, bool, value) =
      if bool then new_array.(modifier) <- value
      else if new_array.(modifier) = value then new_array.(modifier) <- 0
      else ()
    in
    List.iter iter li;
    Entity (nature, new_array)
  in
  match ext_binary with
  | Ext_Entity t -> Entity t
  | Sparse_Entity t -> handle_sparse_list t
