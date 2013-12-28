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

val error_box : title:string -> text:string -> unit

val lexing_error_box : string -> int -> exn -> unit

val general_error_box : exn -> unit

val font_box :
  parent:#GWindow.window_skel ->
  unit -> string option

val process_box :
  parent:#GWindow.window_skel ->
  ruleset:Data_set.rule_set ->
  unit -> (int option * int option * string option) option

val choose_box :
  parent:#GWindow.window_skel ->
  filters:GFile.filter list -> title:string -> unit -> string option

val choose_box_with_script :
  parent:#GWindow.window_skel ->
  scripts:Converter.script list ->
  filters:GFile.filter list ->
  title:string -> unit -> string option * Converter.script

val multi_converter_box :
  parent:#GWindow.window_skel ->
  scripts:Converter.script list ->
  unit -> (Converter.script * (int * Converter.script) list) option

val converter_box :
  parent:#GWindow.window_skel ->
  scripts:Converter.script list -> unit -> Converter.script option

val progress_box : unit -> (unit -> unit) * (unit -> unit) * (float -> unit)

val save_box :
  parent:#GWindow.window_skel ->
  title:string -> unit -> string option