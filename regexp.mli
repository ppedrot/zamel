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
(** The base character set to be used. *)

type pattern = Binary.extended_t

type group = Number of int | Start | Match | End | Dynamic

type expr =
| Atom of pattern
| Seq of expr list
| Opt of expr
| Str of expr
| Grp of expr * group

type template = item list
and item = Char of pattern | Group of (int * template option)

type automaton

val simplify : expr -> expr

val compile : starts:bool -> ends:bool -> expr -> automaton

val process : automaton -> atom list -> template -> atom list