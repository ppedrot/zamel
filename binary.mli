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

exception Misplaced_modifier

type sign =
| Plus
| Minus
| Variable

type elt
(** Type elt represents modifiers. *)

type t
(** Type t is the base type for phonetic representation. It is a immutable,
    abstract type. An element of type t is typically a phoneme or suprasegmental
    from the IPA. *)

type extended_t
(** An element of type extended_t is a regexp pattern, i.e. a partial
    representation using binary features or fully qualified one. Also
    immutable. *)

val match_against : t -> extended_t -> bool
(** [match_against t e] returns [true] if [t] shares the properties described
    by [e], [false] otherwise. *)

val apply : t -> (extended_t * t option) -> t
(** [apply t (e, h_opt)] gives a new element, such the properties described in
    [e] replace those of [t]. [h_opt] is the optional homorganic element, that
    may be needed if [e] has an homorganic field. Raises [Failure "apply"] if
    something goes wrong, like missing required homorganic field, or applying
    suprasegmental modifications to a phoneme or such. *)

val extend : t -> extended_t
(** [extend] transforms plain elements in their pattern couterpart. *)

val unextend : extended_t -> t
(** Reverse application of [extend]. Raises [Invalid_argument "unextend"] if
    the argument is not a fully qualified phonetic element. *)

val from_char : int -> t

val from_modifier : int -> elt

val from_phoneme_list : (sign * string) list -> extended_t

val from_suprasegmental_list : (sign * string) list -> extended_t

val modify : t -> elt list -> t

val represent_binary_list : Converter.script -> t list -> int array
(** [represent_binary c w] writes out word w (noted as a t list) to a ustring
    according to script c. The algorithm is greedy ans is assured to give the
    longest match possible from the start. Raises [Converter.Unknown_sequence]
    if no possible match ever exists. *)

val represent_extended_list : Converter.script -> extended_t list -> int array
(** Same as [represent_binary] except that it deals with patterns element.
    The algorithm cuts the word into lists of continuous fully qualified
    elements separated by groups of partially defined ones. *)
