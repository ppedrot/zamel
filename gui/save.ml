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

open Converter
open Data_set
open Gui_tools

let save lexicon ruleset results file =
  let chan_opt =
    try Some (open_out file)
    with _ -> None
  in match chan_opt with
  | None ->
    let (title, text) = "Error", Printf.sprintf
      "Couldn't open the file %s for writing." file
    in
    Dialog.error_box ~title ~text
  | Some chan ->
    let printf x = Printf.fprintf chan x in
    let script_fun i =
      let rec aux accu = function
      | [] -> accu
      | (date, script) :: q ->
        if i < date then accu
        else aux script q
      in
      match results#script_fun with
      (init, list) -> aux init list
    in

    let min_date, max_date, dialect = results#properties in

    let may_string f = function
    | None -> "None"
    | Some x -> f x
    in

    let min_d = match min_date with
    | None -> min_int
    | Some x -> x
    in

    let id = fun (x:string) -> x in

    let init_scr, scr_list = results#script_fun in

    let iter_script (date, script) =
      printf "\t→ %s (from %i)" script#name date
    in

    let iter_result result =
      let original = result.original in
      let map (w, r, i) =
      (
        represent_word (script_fun i) w,
        represent_rule (script_fun i) r,
        string_of_int i
      )
      in
      let length s =
        Utf8.compute_len s 0 (String.length s)
      in
      let history = List.rev_map map result.history in
      let fold (wl, rl, il) (w, r, i) =
        max wl (length w),
        max rl (length r),
        max il (length i)
      in
      let (wl, rl, il) =
        List.fold_left fold (0, 0, 0) history
      in
      let pad n =
        for i = 1 to n do printf "%s" " " done
      in
      let iter_aux (w, r, i) =
        printf "%s" w;
        pad (wl - length w);
        printf "    ";
        printf "%s" r;
        pad (rl - length r);
        printf "    ";
        pad (il - length i);
        printf "%s" i;
        printf "\n"
      in
      printf "%s\n" (represent_word (script_fun min_d) original);
      List.iter iter_aux history;
      printf "\n"
    in

    printf "########################################\n";
    printf "Lexicon: %s\n" (Filename.basename lexicon#file);
    printf "Ruleset: %s\n" (Filename.basename ruleset#file);
    printf "\n";
    printf "Dialect: %s\n" (may_string id dialect);
    printf "From: %s\n" (may_string string_of_int min_date);
    printf "To: %s\n" (may_string string_of_int max_date);
    printf "Script:\n";
    printf "\t%s\n" init_scr#name;
    List.iter iter_script scr_list;
    printf "########################################\n\n";

    may (List.iter iter_result) results#data;

    close_out chan