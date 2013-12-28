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

let may f = function
| None -> None
| Some x -> Some (f x)

let error_box ~title ~text =
  let window = GWindow.dialog
    ~modal:true
    ~title
    ()
  in
  let _ = GMisc.label
    ~xpad:10
    ~line_wrap:true
    ~justify:`FILL
    ~text
    ~packing:window#vbox#add
    ()
  in
  let button = GButton.button
    ~stock:`OK
    ~packing:(window#action_area#pack ~expand:true)
    ()
  in
  button#connect#clicked ~callback:window#destroy;
  button#grab_default ();
  window#connect#destroy ~callback:GMain.Main.quit;
  window#set_position `CENTER;
  window#show ();
  GMain.Main.main ()

let lexing_error_box f l exn =
  let title, text = match exn with
  | Ulexing.Error | Parsing.Parse_error ->
    "Lexing/Parsing error", Printf.sprintf
    "File %s, line %i: A lexing or parsing error occured." f l
  | Unknown_sequence s ->
    "Unknown sequence", Printf.sprintf
    "File %s, line %i: The sequence '%s' can't be converted with \
    the current script." f l s
  | Unknown_dialect d ->
    "Unknown dialect", Printf.sprintf
    "File %s: The dialect '%s' is not defined." f d
  | Unknown_rule_group g ->
    "Unknown rule group", Printf.sprintf
    "File %s: The group '%s' is not defined." f g
  | Too_many_sections s ->
    "Too many sections", Printf.sprintf
    "File %s: There are too many sections '%s'." f s
  | Incoherent_dates ->
    "Incoherent dates", Printf.sprintf
    "File %s: Some dates are seemingly misordered." f
  | Missing_section_rules ->
    "Missing rule section", Printf.sprintf
    "File %s: The section 'Rules' is missing from the rule set." f
  | Binary.Misplaced_modifier ->
    "Misplaced modifier", Printf.sprintf
    "File %s, line %i: A modifier is misplaced." f l
  | Ipadata.Unknown_char c ->
    let s = Converter.from_int_array [|c|] in
    "Unknown char", Printf.sprintf
    "File %s, line %i: The character '%s' (0x%04X) is not defined." f l s c
  | Ipadata.Unknown_feature s ->
    "Unknown feature", Printf.sprintf
    "File %s, line %i: The binary feature '%s' is not defined." f l s
  | Failure s ->
    "Uncaught exception", Printf.sprintf
    "File %s, line %i: Failed with \"%s\"" f l s
  | _ ->
    "Error", Printf.sprintf
    "File %s, line %i: An unknown error occured." f l
  in
  error_box ~title ~text

let general_error_box exn =
  let title, text = match exn with
  | Xml.File_not_found f ->
    "Missing file", Printf.sprintf
    "The file %s was not found." f
  | Xml.Error (_, pos) ->
    let l = Xml.line pos
    and c1, c2 = Xml.range pos in
    "XML Error", Printf.sprintf
    "An XML parsing error occured line %i, \
    characters %i-%i." l c1 c2
  | Unknown_sequence s ->
    "Unknown sequence", Printf.sprintf
    "The sequence '%s' can't be converted with \
    the current script." s
  | Unappliable_rule (r, w) ->
    let rule = represent_rule ipa_script r
    and word = represent_word ipa_script w in
    "Unappliable rule", Printf.sprintf
    "The rule %s couldn't be applied on the word %s." rule word
  | Failure s ->
    "Uncaught exception", Printf.sprintf
    "Failed with \"%s\"" s
  | _ ->
    "Error",
    "An unknown error occured."
  in
  error_box ~title ~text

let font_box ~parent () =
  let dialog = GWindow.font_selection_dialog
    ~parent
    ~title:"Select a font" ()
  in match dialog#run () with
  | `OK | `APPLY ->
    let font = dialog#selection#font_name in
    dialog#destroy ();
    Some font
  | `CANCEL | `DELETE_EVENT ->
    dialog#destroy ();
    None

let process_box ~parent ~ruleset () =
  let dialog = GWindow.dialog
    ~parent
    ~title:"Select options" ()
  in
  dialog#add_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;

  let table = GPack.table
    ~columns:3
    ~rows:3
    ~packing:dialog#vbox#pack ()
  in

  table#set_col_spacing 0 10;

  let button text top =
    GMisc.label
      ~text
      ~packing:(table#attach ~left:0 ~top) ();
    GButton.check_button
      ~packing:(table#attach ~left:2 ~top) ()
  in
  let b_min = button "From date (inclusive)" 0
  and b_max = button "To date (not inclusive)" 1
  and b_dlc = button "Dialect" 2 in

  let dates =
    List.map (fun (i, _) -> string_of_int i) (get_rules ruleset)
  and dialects =
    List.map snd (get_dialects ruleset)
  in

  let c_min = GEdit.combo_box_text
    ~strings:dates
    ~packing:(table#attach ~expand:`X ~left:1 ~top:0) ()
  in
  let c_max = GEdit.combo_box_text
    ~strings:dates
    ~packing:(table#attach ~expand:`X ~left:1 ~top:1) ()
  in
  let c_dlc = GEdit.combo_box_text
    ~strings:dialects
    ~packing:(table#attach ~expand:`X ~left:1 ~top:2) ()
  in

  let set_state value combo =
    (fst combo)#misc#set_sensitive value;
    if value then (fst combo)#set_active 0
    else (fst combo)#set_active (-1)
  in

  b_min#connect#toggled
    ~callback:(fun () -> set_state b_min#active c_min);
  b_max#connect#toggled
    ~callback:(fun () -> set_state b_max#active c_max);
  b_dlc#connect#toggled
    ~callback:(fun () -> set_state b_dlc#active c_dlc);
  b_dlc#misc#set_sensitive (dialects <> []);

  List.iter (set_state false) [c_min; c_max; c_dlc];

  match dialog#run () with
  | `CANCEL | `DELETE_EVENT ->
    dialog#destroy ();
    None
  | `OK ->
    let get = GEdit.text_combo_get_active in
    let rec assoc l x = match l with
    | [] -> failwith "assoc"
    | (a, b) :: q ->
      if b = x then a else assoc q x
    in
    let min_date =
      may int_of_string (get c_min)
    and max_date =
      may int_of_string (get c_max)
    and dialect =
      may (assoc (get_dialects ruleset)) (get c_dlc)
    in
    dialog#destroy ();
    Some (min_date, max_date, dialect)

class converter_combo list = object (self)

  initializer
    let _ = GMisc.label
      ~justify:`LEFT
      ~text:"Select a script:"
      ~packing:self#pack ()
    in
    let combobox = fst self#combo in
    combobox#set_active 0;
    self#pack combobox#coerce;
    let callback () = match GEdit.text_combo_get_active self#combo with
    | None -> ()
    | Some s -> self#set (List.find (fun x -> x#name = s) list)
    in
    combobox#connect#changed ~callback;
    ()

  val mutable converter = ipa_script

  val combo = GEdit.combo_box_text
    ~strings:(List.map (fun x -> x#name) list)
    ()

  val box = GPack.vbox ()

  method coerce = box#coerce

  method private pack obj = box#pack obj

  method private combo = combo

  method private set c = converter <- c

  method get = converter

end

let choose_box_skel ?extra_widget ~parent ~filters ~title () =
  let dialog = GWindow.file_chooser_dialog
    ~parent
    ~title
    ~action:`OPEN ()
  in
  may dialog#vbox#pack extra_widget;
  List.iter dialog#add_filter filters;
  dialog#add_button_stock `CANCEL `DELETE_EVENT;
  dialog#add_select_button_stock `OPEN `OPEN;
  match dialog#run () with
  | `OPEN ->
    let ans = dialog#filename in
    dialog#destroy ();
    ans
  | `DELETE_EVENT ->
    dialog#destroy ();
    None

let choose_box ~parent ~filters ~title () =
  choose_box_skel ~parent ~filters ~title ()

let choose_box_with_script ~parent ~scripts ~filters ~title () =
  let combo = new converter_combo scripts in
  let file = choose_box_skel
    ~extra_widget:combo#coerce
    ~filters
    ~parent
    ~title ()
  in
  file, combo#get

class multi_converter_combo (scripts: Converter.script list) = object (self)

  initializer
    self#widget#add_with_viewport self#table#coerce;
    self#combo#set_active 0;
    self#table#attach ~left:0 ~top:0 ~expand:`X self#combo#coerce

  val widget = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC
    ~vpolicy:`ALWAYS ()

  val table = GPack.table ~columns:3 ()

  val combo = GEdit.combo_box_text
    ~strings:(List.map (fun x -> x#name) scripts)
    ()

  val mutable id = 1
  val mutable store = []

  method private widget = widget
  method private table : GPack.table = table (* Need typing ??? *)
  method private combo = (fst combo)

  method coerce = widget#coerce

  method add_widget () =
    let combo = GEdit.combo_box_text
      ~strings:(List.map (fun x -> x#name) scripts)
      ~packing:(table#attach ~top:id ~left:0 ~expand:`X) ()
    in
    (fst combo)#set_active 0;
    let adjustment = GData.adjustment
      ~lower:(float min_int)
      ~upper:(float max_int)
      ~step_incr:1.
      ~page_incr:10.
      ~page_size:0.
      ()
    in
    let spin_button = GEdit.spin_button
      ~snap_to_ticks:true
      ~adjustment
      ~packing:(table#attach ~top:id ~left:1) ()
    in
    let button = GButton.tool_button
      ~stock:`REMOVE ()
    in
    table#attach ~top:id ~left:2 button#coerce;
    let callback =
      let curr_id = id in (fun () ->
      table#remove (fst combo)#coerce;
      table#remove spin_button#coerce;
      table#remove button#coerce;
      store <- List.remove_assoc curr_id store)
    in
    let get_date () =
      spin_button#value_as_int
    and get_script () =
      let name = match GEdit.text_combo_get_active combo with
      | None -> "IPA"
      | Some s -> s
      in
      List.find (fun x -> x#name = name) scripts
    in
    store <- (id, (get_date, get_script)) :: store;
    id <- succ id;
    button#connect#clicked ~callback;
    ()

   method get =
    let init_script =
      let name = match GEdit.text_combo_get_active combo with
      | None -> "IPA"
      | Some s -> s
      in
      List.find (fun x -> x#name = name) scripts
    in
    let map (i, (f, g)) = (f(), g()) in
    let ans = List.rev_map map store in
    let rec test accu = function
    | [] -> ()
    | (d, s) :: q ->
      if d < accu then raise Incoherent_dates
      else test d q
    in
    test min_int ans;
    init_script, ans

end

let multi_converter_box ~parent ~scripts () =
  let combo = new multi_converter_combo scripts in
  let dialog = GWindow.dialog ~parent ~title:"Select a set of scripts" () in
  dialog#add_button_stock `ADD `ADD;
  dialog#add_button_stock `CANCEL `DELETE_EVENT;
  dialog#add_button_stock `OK `OK;
  dialog#vbox#pack ~expand:true combo#coerce;
  let rec run () = match dialog#run () with
  | `ADD ->
    combo#add_widget ();
    run ()
  | `OK ->
    let ans =
      try Some combo#get
      with Incoherent_dates -> None
    in
    begin match ans with
    | None ->
      error_box
        ~title:"Misordered dates"
        ~text:"The entered dates are not chronologically ordered.";
      run ()
    | Some x ->
      dialog#destroy ();
      Some x
    end
  | `DELETE_EVENT ->
    dialog#destroy ();
    None
  in
  run ()

let converter_box ~parent ~scripts () =
  let dialog = GWindow.dialog
    ~parent
    ~title:"Select a script" ()
  in
  let combo = new converter_combo scripts in
  dialog#vbox#pack ~expand:true combo#coerce;
  dialog#add_button_stock `CANCEL `DELETE_EVENT;
  dialog#add_button_stock `OK `APPLY;
  match dialog#run () with
  | `APPLY ->
    dialog#destroy ();
    Some combo#get
  | `DELETE_EVENT ->
    dialog#destroy ();
    None

let progress_box () =
  let dialog = GWindow.dialog
    ~no_separator:true
    ~modal:true
    ~title:"In progress" ()
  in
  let bar = GRange.progress_bar
    ~packing:dialog#vbox#pack ()
  in
  let set_fraction f =
    let text = Printf.sprintf "%.0f%%" (100. *. f) in
    bar#set_text text;
    bar#set_fraction f
  in
  dialog#show, dialog#destroy, set_fraction

let save_box ~parent ~title () =
  let dialog = GWindow.file_chooser_dialog
    ~parent
    ~title
    ~action:`SAVE ()
  in
  dialog#add_button_stock `CANCEL `DELETE_EVENT;
  dialog#add_select_button_stock `SAVE `SAVE;
  let rec run () = match dialog#run () with
  | `SAVE ->
    let ans = dialog#filename in
    begin match ans with
    | None ->
      dialog#destroy ();
      None
    | Some f ->
      if Sys.file_exists f then
      (
        let title, text =
          "File already exists",
          "The file already exists. Do you really want \
          to overwrite it?"
        in
        let confirm = GWindow.dialog
          ~parent
          ~title ()
        in
        GMisc.label ~text ~packing:confirm#vbox#pack ();
        confirm#add_button_stock `OK `OK;
        confirm#add_button_stock `CANCEL `DELETE_EVENT;
        match confirm#run () with
        | `OK ->
          confirm#destroy ();
          dialog#destroy ();
          Some f
        | `DELETE_EVENT ->
          confirm#destroy ();
          run ()
      )
      else (Some f)
    end
  | `DELETE_EVENT ->
    dialog#destroy ();
    None
  in run ()
