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

open Gui_tools
open Dialog
open Data_set

let dataset = ref "data/ipadata.xml"

let xml_error f = function
| Xml.File_not_found f ->
  "File not found", Printf.sprintf
  "XML binary system file %s not found!" f
| Xml.Error (_, pos) ->
  let l = Xml.line pos
  and (x, y) = Xml.range pos
  in
  "Malformed XML", Printf.sprintf
  "The XML binary system file %s is malformed: line %i, \
  characters %i-%i." f l x y
| _ ->
  "Error", Printf.sprintf
  "Unknown XML error in file %s." f

let data_init () =
  try Ipadata.init !dataset
  with exn ->
    let default = " No binary phonetic system defined. \
    You must provide a valid XML binary system file in order \
    to run the program."
    and (title, specific) = xml_error !dataset exn in
    error_box ~title ~text:(specific ^ default)

let _ = data_init ()

let page_box (notebook: GPack.notebook) text =
  let append obj =
    let label = GMisc.label ~text () in
    notebook#append_page ~tab_label:label#coerce obj;
    ()
  in
  let scroll = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC
    ~vpolicy:`AUTOMATIC
    ~packing:append
    ()
  in
  GPack.vbox
    ~packing:scroll#add_with_viewport
    ()

class main = object (self)

  initializer

    let main_box = GPack.vbox ~packing:self#window#add () in
    main_box#pack self#menu_bar#coerce;
    let notebook = GPack.notebook
      ~packing:(main_box#pack ~expand:true) ()
    in
    (page_box notebook "Lexicon")#pack ~expand:true self#lexicon_view#coerce;
    (page_box notebook "Ruleset")#pack ~expand:true self#ruleset_view#coerce;
    (page_box notebook "Results")#pack ~expand:true self#results_view#coerce;
    self#window#connect#destroy ~callback:GMain.Main.quit;

    let file_menu = create_menu (`M ("File",
    [
      `Bi("Open a lexicon", `OPEN, self#set_lexicon);
      `Bi("Open a ruleset", `OPEN, self#set_ruleset);
      `Bi("Save results", `SAVE, self#save);
      `S;
      `Bi("Add a converter", `ADD, self#add_converter);
      `Bi("Clear converters", `REFRESH, self#clear_converters);
      `S;
      `St(`QUIT, GMain.Main.quit)
    ]
    ))
    and option_menu = create_menu (`M ("Option",
    [
      `St(`PREFERENCES, fun () -> ());
      `S;
      `Bi("Change binary system", `EXECUTE, self#change_binary);
    ]
    ))
    in
    let lexicon_popup = create_popup
    [
      `Bi ("Open lexicon", `OPEN, self#set_lexicon);
      `Bi ("Set script", `BOLD, self#set_lexicon_script);
      `Bi ("Set font", `SELECT_FONT, self#set_lexicon_font);
    ]
    and ruleset_popup = create_popup
    [
      `Bi ("Open ruleset", `OPEN, self#set_ruleset);
      `Bi ("Set script", `BOLD, self#set_ruleset_script);
      `Bi ("Set font", `SELECT_FONT, self#set_ruleset_font);
    ]
    and results_popup = create_popup
    [
      `Bi ("Process results", `EXECUTE, self#set_results);
      `Bi ("Save results", `SAVE, self#save);
      `B ("Toggle date display", self#toggle_show_date);
      `Bi ("Set script", `BOLD, self#set_results_script);
      `Bi ("Set font", `SELECT_FONT, self#set_results_font);
    ]
    in

    let button_pressed menu ev =
      if GdkEvent.Button.button ev = 3 then
      (
        menu#popup ~button:3 ~time:(GdkEvent.Button.time ev);
        true
      )
      else
        false
    in

    self#lexicon_view#event#connect#button_press
      ~callback:(button_pressed lexicon_popup);
    self#ruleset_view#event#connect#button_press
      ~callback:(button_pressed ruleset_popup);
    self#results_view#event#connect#button_press
      ~callback:(button_pressed results_popup);

    self#menu_bar#add file_menu;
    self#menu_bar#add option_menu;

    self#window#show ()

  val window = GWindow.window ~title:"Zamel" ()
  val menu_bar = GMenu.menu_bar ()

  val lexicon_view = new lexicon_view
  val ruleset_view = new ruleset_view
  val results_view = new results_view
  val script_list = new script_list

  method menu_bar = menu_bar
  method window = window

  method scripts = script_list#get

  method lexicon_view = lexicon_view
  method ruleset_view = ruleset_view
  method results_view = results_view

  method set_lexicon () =
    let (file, script) = choose_box_with_script
      ~scripts:script_list#get
      ~parent:window
      ~title:"Choose a lexicon"
      ~filters:[lexicon_filter; any_filter] ()
    in
    may (lexicon_view#set_data script) file;
    results_view#clear_data;
    ()

  method set_lexicon_script () =
    let script = converter_box
      ~parent:window
      ~scripts:script_list#get ()
    in
    may lexicon_view#set_script script;
    ()

  method set_lexicon_font () =
    let font = font_box
      ~parent:window ()
    in
    may lexicon_view#set_font font;
    ()

  method set_ruleset () =
    let (file, script) = choose_box_with_script
      ~scripts:script_list#get
      ~parent:window
      ~title:"Choose a ruleset"
      ~filters:[ruleset_filter; any_filter] ()
    in
    may (ruleset_view#set_data script) file;
    results_view#clear_data;
    ()

  method set_ruleset_script () =
    let script = converter_box
      ~parent:window
      ~scripts:script_list#get ()
    in
    may ruleset_view#set_script script;
    ()

  method set_ruleset_font () =
    let font = font_box
      ~parent:window ()
    in
    may ruleset_view#set_font font;
    ()

  method set_results () =
    match lexicon_view#data, ruleset_view#data with
    | Some l, Some r ->
      let options = process_box
        ~parent:window
        ~ruleset:r ()
      in
      let _ = match options with
      | None -> ()
      | Some (min_d, max_d, dlc) ->
        results_view#set_data l r min_d max_d dlc
      in
      ()
    | None, _ ->
      error_box
        ~title:"No lexicon"
        ~text:"There is currently no lexicon set."
    | _, None ->
      error_box
        ~title:"No ruleset"
        ~text:"There is currently no ruleset set."

  method set_results_script () =
    let script_fun = multi_converter_box
      ~parent:window
      ~scripts:script_list#get ()
    in
    may results_view#set_script_fun script_fun;
    ()

  method set_results_font () =
    let font = font_box
      ~parent:window ()
    in
    may results_view#set_font font;
    ()

  method toggle_show_date () =
    results_view#toggle_show_date ()

  method add_converter () =
    let file = choose_box
      ~parent:window
      ~title:"Choose a script"
      ~filters:[script_filter; any_filter] ()
    in
    may script_list#add file;
    ()

  method clear_converters () =
    let ipa = Converter.ipa_script in
    lexicon_view#set_script ipa;
    ruleset_view#set_script ipa;
    results_view#set_script ipa;
    script_list#clear

  method change_binary () =
    let file = choose_box
      ~parent:window
      ~title:"Choose a script"
      ~filters:[xml_filter; any_filter] ()
    in
    let () = match file with
    | None -> ()
    | Some f ->
      lexicon_view#clear_data;
      ruleset_view#clear_data;
      results_view#clear_data;
      try
        Ipadata.init f
      with exn ->
        let default = " No binary phonetic system defined."
        and (title, specific) = xml_error f exn in
        error_box ~title ~text:(specific ^ default)
    in ()

  method save () =
    match results_view#data with
    | None ->
      let title, text = "No results computed",
      "The result list is empty. There is nothing to save."
      in
      error_box ~title ~text
    | _ ->
      let file = save_box
        ~parent:window
        ~title:"Choose a save file" ()
      in
      let f = Save.save
        lexicon_view ruleset_view results_view
      in
      may f file;
      ()

end

let _ =
  new main;
  GtkThread.main ()