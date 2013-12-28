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

type atom = Binary.t
type pattern = Binary.extended_t

type group =
| Number of int
| Start
| Match
| End
| Dynamic

type expr =
| Atom of pattern
| Seq of expr list
(* | Alt of expr list *)
| Opt of expr
| Str of expr
| Grp of expr * group

type template = item list
and item =
| Char of pattern
| Group of (int * template option)

type automaton =
{
  delta: (pattern * int list list) array;
  group: (int * group) option array;
  start: (int * int list list) list;
  greed: bool;
  first: bool;
}

let ( =~ ) = Binary.match_against
let ( ~> ) = Binary.unextend
let ( +> ) = Binary.apply

let dummy_char = Binary.from_phoneme_list []

let length expr =
  let rec aux accu = function
  | [] -> accu
  | Atom _ :: q -> aux (accu + 2) q
  | Seq l :: q -> aux accu (List.rev_append l q)
(*   | Alt l :: q -> aux (accu + 2) (List.rev_append l q) *)
  | Opt e :: q -> aux accu (e :: q)
  | Str e :: q -> aux accu (e :: q)
  | Grp (e, _) :: q -> aux (accu + 2) (e :: q)
  in
aux 0 [expr]

let rec simplify = function
| Atom a -> Atom a
| Seq l ->
  let fold accu = function
  | Seq k ->
    (List.rev_map simplify k) @ accu
  | f -> (simplify f) :: accu
  in
  Seq (List.rev (List.fold_left fold [] l))
| Opt e -> Opt (simplify e)
| Str e -> Str (simplify e)
| Grp (e, g) -> Grp (simplify e, g)

let compile ~starts ~ends expr =

  let l = length expr in
  let delta = Array.make l (dummy_char, []) in
  let group = Array.make l None in
  let group_back = Array.make l false in
  let delta_1 = Array.make l None in
  let delta_2 = Array.make l [] in
  let closure = Array.make l [] in
  let off = ref 0 in

  let rec apply = function
  | Atom a ->
    delta_1.(!off) <- Some a;
    incr off
  | Seq l ->
    let rec aux = function
    | [] -> decr off
    | [t] -> apply t
    | t::q ->
      apply t;
      delta_2.(!off) <- (!off + 1) :: delta_2.(!off);
      incr off;
      aux q
    in
    aux l
(*  | Alt l ->
    let soff = !off in
    let eoff = !off + length (Alt l) - 1 in
    incr off;
    let iter expr =
      delta_2.(soff) <- (!off) :: delta_2.(soff);
      apply expr;
      delta_2.(!off) <- (eoff) :: delta_2.(!off);
      incr off
    in
    List.iter iter l*)
  | Opt expr ->
    let eoff = !off + length expr - 1 in
    delta_2.(!off) <- (eoff) :: delta_2.(!off);
    apply expr
  | Str expr ->
    let eoff = !off + length expr - 1 in
    delta_2.(!off) <- (eoff) :: delta_2.(!off);
    delta_2.(eoff) <- (!off) :: delta_2.(eoff);
    apply expr
  | Grp(expr, grp) ->
    let eoff = !off + length expr in
    delta_2.(!off) <- (!off + 1) :: delta_2.(!off);
    delta_2.(eoff) <- (eoff + 1) :: delta_2.(eoff);
    group.(eoff + 1) <- Some (!off, grp);
    group_back.(!off) <- true;
    group_back.(eoff + 1) <- true;
    incr off;
    apply expr;
    incr off (* ? *)
  in
  apply expr;

  let close i =
    let rec insert i t = function
    | [] -> [i, t]
    | (j, s)::q ->
      if j > i then (i, t)::(j, s)::q
      else if j < i then (j, s)::(insert i t q)
      else (j, s)::q
    in
    let break = ref true in
    let slist = ref [i, []] in
    while !break do
      let next = List.fold_left (fun accu1 (j, s) ->
        List.fold_left (fun accu2 x -> (x, (j::s)) :: accu2) accu1 delta_2.(j))
        [] !slist
      in
      let nlist = List.fold_left
        (fun accu (j, s) -> insert j s accu) !slist next
      in
      if nlist = !slist then break := false else slist := nlist;
    done;
    List.filter (fun (j, _) -> (j = l - 1) || (delta_1.(j) <> None)) !slist
  in

  for i = 0 to l - 1 do
    closure.(i) <- close i;
    let map (j, s) = (j, List.filter (fun i -> group_back.(i)) s) in
    closure.(i) <- List.map map closure.(i)
  done;

  for i = 0 to l - 1 do
    match delta_1.(i) with
    | None -> ()
    | Some a ->
      let closure = closure.(i+1) in
      let next = List.map (fun (j, s) -> j :: s) closure in
      delta.(i) <- (a, next)
  done;

  {
    delta = delta;
    group = group;
    start = List.rev_map (fun (i, s) -> (i, [i::s])) closure.(0);
    greed = ends;
    first = starts
  }

let rec insert i t = function
| [] -> [i, t]
| (j, s)::q ->
  if j < i then (i, t)::(j, s)::q
  else if j > i then (j, s)::(insert i t q)
  else (j, min s t)::q

let map (f: int -> int list list) l =
  let fold accu1 (s, l) =
    let next = f s in
    List.fold_left
      (fun accu2 ->
        (function
        | [] -> accu2
        | t::q -> insert t ((t :: q) :: l) accu2
        )
      ) accu1 next
  in
List.fold_left fold [] l

let execute rx list =
  let last = (Array.length rx.delta) - 1 in
  let check state_list =
    if rx.greed then false
    else match state_list with
    | (x, _) :: _ -> x = last
    | _ -> false

  in
  let rec fold accu matched_accu = function
  | [] -> accu, matched_accu, []
  | t::q as l ->
    let accu =
      if rx.first then accu
      else List.fold_left (fun l (j, s) -> insert j s l) accu rx.start
    in
    if check accu then
      (* Se ni atingis la finan staton dum malĝisfina serĉado *)
      accu, matched_accu, l
    else if accu = [] then
      (* Se la statlisto estas maplena, t.e. la aŭtomato estas senmova *)
      [], [], []
    else
      (* Kontraŭkaze, ni antaŭiru! *)
      let mapper i =
        let to_match, next = rx.delta.(i) in
        if t =~ to_match then next else []
      in
      fold (map mapper accu) (t :: matched_accu) q
  in
  let result, matched, left = fold rx.start [] list in
  try
    let _, ans = List.find (fun (x, _) -> x = last) result in
    ans, matched, left
  with Not_found -> [], [], list

let find rx (list: atom list) result =
  let add_list x list = List.map (fun (grp, l) -> (grp, x :: l)) list in
  (* Ni uzas 3 akumulilojn: ans estas por la plenumitaj grupoj, accu estas por
     la plenumataj grupoj kaj accu_to estas por la serĉataj grupfinaj statoj
     rilataj al la grupoj en accu. *)
  let rec find_aux ans accu accu_to grp_to list = function
  | [] -> []
  | t :: q ->
    let rec fold ans accu accu_to grp_to = function
    | [] -> ans, accu, accu_to, grp_to
    | s :: r ->
      begin match rx.group.(s) with
      | None ->
        if s = grp_to then
          let matched = List.hd accu and naccu = List.tl accu in
          let ngrp_to = List.hd accu_to and naccu_to = List.tl accu_to in
          fold (matched :: ans) naccu naccu_to ngrp_to r
        else
          fold ans accu accu_to grp_to r
      | Some (ngrp_to, grp) ->
        fold ans ((grp, []) :: accu) (grp_to :: accu_to) ngrp_to r
      end
    in
    let nans, naccu, naccu_to, ngrp_to = fold ans accu accu_to grp_to t in
    begin match q with
    | [] -> (Start, List.rev_append list (List.assoc Start nans)) :: nans
    | _ ->
      let atom = List.hd list and nlist = List.tl list in
      find_aux nans (add_list atom naccu) naccu_to ngrp_to nlist q
    end
  in
find_aux [] [] [] (-1) list result

let find_all rx list =
  if rx.greed || rx.first then
    let result, matched, left = execute rx list in
    [find rx matched result], left
  else
    let rec fold accu list =
      let result, matched, left = execute rx list in
      if left = list then
        if result = [] then
          accu, left
        else
          ((find rx matched result) :: accu), left
      else
        fold ((find rx matched result) :: accu) left
    in
    fold [] list

let apply submatch template =
  let homorganic =
    try Some (List.hd (List.assoc Dynamic submatch))
    with _ -> None
  in
  let rec aux_apply accu word = function
  | [] -> accu
  | (Char pattern) :: q ->
    begin match word with
    | [] -> aux_apply ((~> pattern) :: accu) [] q
    | atom :: t ->
      aux_apply ((atom +> (pattern, homorganic)) :: accu) t q
    end
  | (Group (number, Some subtemplate)) :: q ->
    let subgroup = if number = 0 then Match else Number number in
    let subword = List.assoc subgroup submatch in
    let subresult = aux_apply accu subword subtemplate in
    aux_apply subresult word q (* FIXME *)
  | (Group (number, None)) :: q ->
    let subgroup = if number = 0 then Match else Number number in
    let subword = List.assoc subgroup submatch in
    aux_apply (List.rev_append subword accu) word q
  in
let start_part = List.assoc Start submatch
and end_part = List.assoc End submatch
and rev_result = aux_apply [] (List.assoc Match submatch) template in
start_part @ (List.rev_append rev_result end_part)

let process rx word template =
  let result, left = find_all rx word in
  let rec aux accu = function
  | [] | [[]] -> List.concat accu
  | t :: q ->
    let next = apply t template in
    aux (next :: accu) q
  in
aux [left] result