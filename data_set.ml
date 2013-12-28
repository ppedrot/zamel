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
open Regexp

exception Unknown_dialect of string
exception Unknown_rule_group of string
exception Incoherent_dates
exception Too_many_sections of string
exception Missing_section_rules

type rule =
{
  automaton: Regexp.automaton;
  rx_matching: Regexp.expr;
  rx_environment: Regexp.expr * Regexp.expr;
  template: Regexp.template;
  anchor: bool * bool;
}

type abstr_rule =
| Rule of (rule * string list)
| Rule_group of (string * string list)

type rule_set =
{
  rules: (int * (abstr_rule list)) list;
  persistent: abstr_rule list;
  groups: (string, rule list) Hashtbl.t;
  dialects: (string, string) Hashtbl.t;
}

type word = Binary.t list

type result = { original : word; history : (word * rule * int) list }

type lexicon = word list

exception Unappliable_rule of (rule * word)

let buf = Buffer.create 64

let store_ustring s = Array.iter (Utf8.store buf) s

let from_int_array s = Utf8.from_int_array s 0 (Array.length s)

let create_rule (is_start, is_end) (before, matching, after) template =
  let g_matching = Grp (matching, Match)
  and g_before = Grp (before, Start)
  and g_after = Grp (after, End) in
  let automaton =
    Regexp.compile is_start is_end (Seq [g_before; g_matching; g_after]) in
  {
    automaton = automaton;
    rx_matching = matching;
    rx_environment = before, after;
    template = template;
    anchor = is_start, is_end;
  }

let enclose bound_action (before, after) main_action arg =
  bound_action before;
  main_action arg;
  bound_action after

let represent_rule converter rule =
  Buffer.clear buf;
  let rec store_expr = function
  | Atom c -> 
    store_ustring (represent_extended_list converter [c])
  | Seq l ->
    let rec iter accu = function
    | [] ->
      store_ustring (represent_extended_list converter (List.rev accu))
    | Atom c :: q ->
      iter (c :: accu) q
    | e :: q ->
      iter accu [];
      store_expr e;
      iter [] q
    in iter [] l
  | Opt e ->
    with_delims converter#paren e
  | Str e ->
    with_delims converter#paren e;
    store_ustring converter#star
  | Grp (e, (Number _ | Dynamic)) ->
    with_delims converter#brace e
  | _ -> ()
  and with_delims delims expr = enclose store_ustring delims store_expr expr
  in

  let rec store_template li = store_template' [] li
  and store_template' accu = function
  | [] ->
    store_ustring (represent_extended_list converter (List.rev accu))
  | Char c :: q ->
    store_template' (c :: accu) q
  | Group (i, Some t) :: q ->
    store_template' accu [];
    enclose store_ustring converter#brace
      (fun s -> Buffer.add_string buf s; store_template t) (string_of_int i);
    store_template q;
  | Group (i, None) :: q ->
    store_ustring (represent_extended_list converter (List.rev accu));
    enclose store_ustring converter#brace (Buffer.add_string buf) (string_of_int i);
    store_template q
  in
  store_expr rule.rx_matching;
  store_ustring converter#sep;
  store_template rule.template;
  store_ustring converter#sep;
  let maybe_anchor p = if p then store_ustring converter#anchor in
  enclose maybe_anchor rule.anchor
    (enclose store_expr rule.rx_environment store_ustring) converter#holder;
  Buffer.contents buf

let represent_word converter word =
  from_int_array (represent_binary_list converter word)

let create_ruleset rules persistent groups dialects =
  let group_hash = Hashtbl.create 0
  and dialect_hash = Hashtbl.create 0 in
  let add hashtbl (key, value) = Hashtbl.add hashtbl key value in
  List.iter (add group_hash) groups;
  List.iter (add dialect_hash) dialects;
  {
    rules = rules;
    persistent = persistent;
    groups = group_hash;
    dialects = dialect_hash
  }

let get_rules ruleset =
  ruleset.rules

let get_persistent ruleset =
  ruleset.persistent

let get_dialects ruleset =
  let fold key value accu = (key, value) :: accu in
  Hashtbl.fold fold ruleset.dialects []

let assoc_dialect ruleset dialect =
  Hashtbl.find ruleset.dialects dialect

let get_groups ruleset =
  let fold key value accu = key :: accu in
  Hashtbl.fold fold ruleset.groups []

let assoc_group ruleset group =
  Hashtbl.find ruleset.groups group

let represent_rule_set converter rule_set = ()

let apply rule date result =
  let word = match result.history with
  | [] -> result.original
  | (w, r, d) :: q -> w
  in
  let ans =
    try process rule.automaton word rule.template
    with _ -> raise (Unappliable_rule (rule, word))
  in
  if ans <> word then
    {
      original = result.original ;
      history = (ans, rule, date) :: result.history
    }
  else
    result

let apply_set ?(progress = fun _ -> ()) ruleset lexicon min_date max_date dialect_opt () =
  let test_date date = match min_date, max_date with
  | None, None -> true
  | Some i, None -> date >= i
  | None, Some j -> date < j
  | Some i, Some j -> (date >= i) && (date < j)
  in
  let test_dialect = function
  | [] -> true
  | l ->
    match dialect_opt with
    | Some d ->
      begin
      try List.mem d l
      with Not_found -> raise (Unknown_dialect d)
      end
    | None -> true
  in
  let word_list =
    let map w = { original = w; history = [] } in
    List.map map lexicon
  in
  let fold func accu (date, subrules) =
    let apply_rule results rule =
      func date results rule
    in
    let apply_abstr results = function
    | Rule (r, d) ->
      if test_dialect d then
        apply_rule results r
      else
        results
    | Rule_group (g, d) ->
      if test_dialect d then
        List.fold_left apply_rule results (assoc_group ruleset g)
      else
        results
    in
    let apply_total results abstr =
      List.fold_left apply_abstr results (abstr :: (get_persistent ruleset))
    in
    if test_date date then
      List.fold_left apply_total accu subrules
    else
      accu
  in

  let len_fun date results rule = succ results in
  let len =
    List.fold_left (fold len_fun) 0 (get_rules ruleset)
  in
  let update =
    let off = ref 1 in
    if len = 0 then
      (fun () -> progress 1.)
    else
      (fun () -> progress (float !off /. float len); incr off)
  in

  let apply_fun date results rule =
    update ();
    List.map (apply rule date) results
  in
  List.fold_left (fold apply_fun) word_list (get_rules ruleset)
