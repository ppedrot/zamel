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

exception Unknown_dialect of string
exception Unknown_rule_group of string
exception Too_many_sections of string
exception Incoherent_dates
exception Missing_section_rules

type word = Binary.t list
type rule
type result = { original : word; history : (word * rule * int) list }

exception Unappliable_rule of (rule * word)

type abstr_rule =
| Rule of (rule * string list)
| Rule_group of (string * string list)

type lexicon = word list
type rule_set

val represent_word : Converter.script -> word -> string

val create_rule :
  bool * bool ->
  Regexp.expr * Regexp.expr * Regexp.expr ->
  Regexp.template -> rule

val represent_rule : Converter.script -> rule -> string

val create_ruleset :
  (int * abstr_rule list) list ->
  abstr_rule list ->
  (string * rule list) list ->
  (string * string) list -> rule_set

val get_rules : rule_set -> (int * abstr_rule list) list

val get_persistent : rule_set -> abstr_rule list

val get_groups : rule_set -> string list

val assoc_group : rule_set -> string -> rule list

val get_dialects : rule_set -> (string * string) list

val assoc_dialect : rule_set -> string -> string

val represent_rule_set : Converter.script -> rule_set -> unit

val apply_set :
  ?progress:(float -> unit) ->
  rule_set ->
  lexicon ->
  int option ->
  int option -> string option -> unit -> result list
