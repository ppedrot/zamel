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

val may : ('a -> 'b) -> 'a option -> 'b option

val any_filter : GFile.filter
val lexicon_filter : GFile.filter
val ruleset_filter : GFile.filter
val script_filter : GFile.filter
val xml_filter : GFile.filter

class preferences : object  end

class lexicon_view :
  object
    method clear_data : unit
    method coerce : GObj.widget
    method event : GObj.event_ops
    method file : string
    method data : Data_set.lexicon option
    method script : Converter.script
    method set_data : Converter.script -> string -> unit
    method set_font : string -> unit
    method set_script : Converter.script -> unit
  end

class ruleset_view :
  object
    method clear_data : unit
    method coerce : GObj.widget
    method event : GObj.event_ops
    method file : string
    method data : Data_set.rule_set option
    method script : Converter.script
    method set_data : Converter.script -> string -> unit
    method set_font : string -> unit
    method set_script : Converter.script -> unit
  end

class results_view :
  object
    method clear_data : unit
    method coerce : GObj.widget
    method data : Data_set.result list option
    method event : GObj.event_ops
    method properties : int option * int option * string option
    method script_fun : Converter.script * (int * Converter.script) list
    method set_data :
      Data_set.lexicon ->
      Data_set.rule_set ->
      int option -> int option -> string option -> unit
    method set_font : string -> unit
    method set_script : Converter.script -> unit
    method set_script_fun :
      Converter.script * (int * Converter.script) list -> unit
    method set_show_date : bool -> unit
    method toggle_show_date : unit -> unit
  end

class script_list :
  object
    method add : string -> unit
    method clear : unit
    method get : Converter.script list
  end

val create_menu :
  ([< `B of string * (unit -> unit)
    | `Bi of string * GtkStock.id * (unit -> unit)
    | `M of string * 'a list
    | `S
    | `St of GtkStock.id * (unit -> unit) ]
   as 'a) ->
  GMenu.menu_item

val create_popup :
  [< `B of string * (unit -> unit)
  | `Bi of string * GtkStock.id * (unit -> unit)
  | `St of GtkStock.id * (unit -> unit)
  | `S ] list -> GMenu.menu
