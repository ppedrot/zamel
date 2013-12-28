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

type tree = item list
and item = Node of (int * int array option * tree)

type dict = { tree: tree; height: int }

let to_int_array s = Utf8.to_int_array s 0 (String.length s)
let from_int_array s = Utf8.from_int_array s 0 (Array.length s)

let is_space c = c = 32 || c = 9

let skip_spaces s =
  let fold accu c = if is_space c then accu else succ accu in
  let l = Array.fold_left fold 0 s in
  let m = Array.length s in
  let ans = Array.make l 0 in
  let i = ref 0 in
  let j = ref 0 in
  while !i < m do
    let c = s.(!i) in
    if is_space c then incr i
    else (ans.(!j) <- c; incr i; incr j);
  done;
  ans

let ipa_script =
  let dup f (left, right) = f left, f right in
  object
    method name = "IPA"
    method paren = dup to_int_array ("(", ")")
    method brace = dup to_int_array ("{", "}")
    method brack = dup to_int_array ("[", "]")
    method chevr = dup to_int_array ("<", ">")
    method star = to_int_array "*"
    method sep = to_int_array "/"
    method holder = to_int_array "_"
    method anchor = to_int_array "#"
    method comment = to_int_array "#"
    method to_ipa = skip_spaces
    method to_script = skip_spaces
  end

let create_dict () = { tree = []; height = 0 }

let add dict seq image =
  let seq = skip_spaces seq in
  let image = skip_spaces image in
  let l = Array.length seq in
  assert (l > 0);
  let constr aux i img_opt subtree =
    if i = l - 1 then
      Node (seq.(i), Some image, subtree)
    else
      Node (seq.(i), img_opt, aux (succ i) subtree)
  in
  let rec insert_list i = function
  | [] ->
    [constr insert_list i None []]
  | Node (chr, img, st) as node :: next ->
    let cmp = seq.(i) - chr in
    if cmp > 0 then node :: (insert_list i next)
    else if cmp < 0 then (constr insert_list i None []) :: node :: next
    else (constr insert_list i img st) :: next
  in
  let n_tree = insert_list 0 dict.tree in
  { tree = n_tree; height = max l dict.height }

let convert dict seq =
  let l = Array.length seq in
  let rec find char = function
  | [] -> raise Not_found
  | Node (c, i, st) :: q ->
    if char < c then raise Not_found
    else if char > c then find char q
    else (i, st)
  in
  let fold c (rep, tree, full) accu =
    try
      match find c tree with
      | None, subtree ->
        (rep, subtree, false) :: accu
      | Some img, subtree ->
        (rep, subtree, false) :: ((img :: rep), dict.tree, true) :: accu
    with Not_found -> accu
  in
  let ans = ref [[], dict.tree, true] in
  for i = 0 to l - 1 do
    if not (is_space seq.(i)) then
      ans := List.fold_right (fold seq.(i)) !ans [];
  done;
  let rec find = function
  | [] -> raise (Unknown_sequence (from_int_array seq))
  | (l, _, true) :: _ -> Array.concat (List.rev l)
  | _ :: q -> find q
  in
  find !ans

let create_script name reserved to_ipa_dict to_script_dict =
  let data = Array.copy reserved in
  let name = String.copy name in
  let get (left, right) = data.(left), data.(right) in
  object
    method name = name
    method paren = get (0, 1)
    method brace = get (2, 3)
    method brack = get (4, 5)
    method chevr = get (6, 7)
    method star = data.(8)
    method sep = data.(9)
    method holder = data.(10)
    method anchor = data.(11)
    method comment = data.(12)
    method to_ipa = convert to_ipa_dict
    method to_script = convert to_script_dict
  end
