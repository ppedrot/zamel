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
open Dialog

type rule_store =
| S_section of string
| S_group_def of string
| S_group of string
| S_date of int
| S_rule of rule

type result_store =
| S_original of (word * word)
| S_history of (word * rule * int)

let may f = function
| None -> None
| Some x -> Some (f x)

let rec create_menu = function
| `B (label, callback) ->
  let item = GMenu.menu_item ~label () in
  item#connect#activate ~callback;
  item
| `Bi (label, stock, callback) ->
  let image = GMisc.image ~stock () in
  let item = GMenu.image_menu_item ~image ~label () in
  item#connect#activate ~callback;
  (item :> GMenu.menu_item)
| `St (stock, callback) ->
  let item = GMenu.image_menu_item ~stock () in
  item#connect#activate ~callback;
  (item :> GMenu.menu_item)
| `M (label, submenu) ->
  let item = GMenu.menu_item ~label () in
  let menu = GMenu.menu ~packing:item#set_submenu () in
  let iter x =
    let subitem = create_menu x in
    menu#add subitem
  in
  List.iter iter submenu;
  item
| `S ->
  GMenu.separator_item ()

let create_popup list =
  let menu = GMenu.menu () in
  let iter = function
  | `B (label, callback) ->
    let item = GMenu.menu_item
      ~packing:menu#add
      ~label ()
    in
    item#connect#activate ~callback;
    ()
  | `Bi (label, stock, callback) ->
    let image = GMisc.image ~stock () in
    let item = GMenu.image_menu_item ~image ~label ~packing:menu#add () in
    item#connect#activate ~callback;
    ()
  | `St (stock, callback) ->
    let item = GMenu.image_menu_item ~stock ~packing:menu#add () in
    item#connect#activate ~callback;
    ()
  | `S ->
    GMenu.separator_item ~packing:menu#add ();
    ()
  in
  List.iter iter list;
  menu

(* File filters *)

let any_filter = GFile.filter
  ~name:"Any file" ~patterns:["*"] ()
let lexicon_filter = GFile.filter
  ~name:"Lexicons (*.lex)" ~patterns:["*.lex"] ()
let ruleset_filter = GFile.filter
  ~name:"Rulesets (*.sc)" ~patterns:["*.sc"] ()
let script_filter = GFile.filter
  ~name:"Scripts (*.orth)" ~patterns:["*.orth"] ()
let xml_filter = GFile.filter
  ~name:"XML files (*.xml)" ~patterns:["*.xml"] ()

(* Treeview/Model related stuff *)

let iter_model iter_fun (model: #GTree.model) =
  let rec aux iter =
    iter_fun model iter;
    if model#iter_has_child iter then
      let child = model#iter_children (Some iter) in
      aux child
    else ();
    if model#iter_next iter then aux iter
    else ()
  in match model#get_iter_first with
  | None -> ()
  | Some iter -> aux iter


let lexicon_cols = new GTree.column_list
let lexicon_col1 = lexicon_cols#add
  (Gobject.Data.caml: word Gobject.data_conv)
let lexicon_col2 = lexicon_cols#add
  (Gobject.Data.string)

let from_lexicon lex =
  let store = GTree.list_store lexicon_cols in
  let iter word =
    let row = store#append () in
    store#set ~row ~column:lexicon_col1 word;
  in
  List.iter iter lex;
  store

let set_lexicon_script script model =
  let iter (model: GTree.list_store) row =
    let data = model#get ~row ~column:lexicon_col1 in
    let text = represent_word script data in
    model#set ~row ~column:lexicon_col2 text
  in
  iter_model iter model

let ruleset_cols = new GTree.column_list
let ruleset_col1 = ruleset_cols#add
  (Gobject.Data.caml: rule_store Gobject.data_conv)
let ruleset_col2 = ruleset_cols#add
  (Gobject.Data.string)
let ruleset_col3 = ruleset_cols#add
  (Gobject.Data.string)

let from_ruleset rul =
  let store = GTree.tree_store ruleset_cols in

  let group_row = store#append ()
  and persistent_row = store#append ()
  and rules_row = store#append ()
  in

  let set1 row data =
    store#set ~row ~column:ruleset_col1 data
  and set3 row data =
    store#set ~row ~column:ruleset_col3 data
  in

  let get_arule = function
  | Rule (r, d) ->
    (S_rule r), (String.concat ", " (List.map (assoc_dialect rul) d))
  | Rule_group (g, d) ->
    (S_group g), (String.concat ", " (List.map (assoc_dialect rul) d))
  in

  set1 group_row (S_section "Groups");
  set1 persistent_row (S_section "Persistent");
  set1 rules_row (S_section "Rules");

  let iter_groups group =
    let parent = store#append ~parent:group_row () in
    set1 parent (S_group_def group);
    let iter_aux rule =
      let row = store#append ~parent () in
      set1 row (S_rule rule)
    in
    List.iter iter_aux (assoc_group rul group)
  in
  List.iter iter_groups (get_groups rul);

  let iter_persistent arule =
    let row = store#append ~parent:persistent_row () in
    let data1, data3 = get_arule arule in
    set1 row data1;
    set3 row data3
  in
  List.iter iter_persistent (get_persistent rul);

  let iter_rules (date, subrules) =
    let parent = store#append ~parent:rules_row () in
    set1 parent (S_date date);
    let iter_aux arule =
      let data1, data3 = get_arule arule in
      let row = store#append ~parent () in
      set1 row data1;
      set3 row data3
    in
    List.iter iter_aux subrules
  in
  List.iter iter_rules (get_rules rul);

  store

let set_ruleset_script script model =
  let iter (model: GTree.tree_store) row =
    let data = model#get ~row ~column:ruleset_col1 in
    let text = match data with
    | S_section s -> s
    | S_group_def g -> g
    | S_group g -> g
    | S_date d -> string_of_int d
    | S_rule r -> represent_rule script r
    in
    model#set ~row ~column:ruleset_col2 text
  in
  iter_model iter model

let results_cols = new GTree.column_list
let results_col1 = results_cols#add
  (Gobject.Data.caml: result_store Gobject.data_conv)
let results_col2 = results_cols#add
  (Gobject.Data.string)
let results_col3 = results_cols#add
  (Gobject.Data.string)
let results_col4 = results_cols#add
  (Gobject.Data.string)

let from_results res =
  let store = GTree.tree_store results_cols in
  let iter result =
    let original = result.original
    and final = match result.history with
    | [] -> result.original
    | (w, r, d) :: q -> w
(*     in *)
    and history = List.rev result.history in
    let parent = store#append () in
    store#set ~row:parent ~column:results_col1
      (S_original (original, final));
    let iter_aux e =
      let row = store#append ~parent () in
      store#set ~row ~column:results_col1
        (S_history e)
    in
    List.iter iter_aux history
  in
  List.iter iter res;

  store

let set_results_script script_func min_date max_date model =
  let iter (model: GTree.tree_store) row =
    let data = model#get ~row ~column:results_col1 in
    match data with
    | S_original (o, f) ->
      model#set ~row ~column:results_col2
        (represent_word (script_func min_date) o);
      model#set ~row ~column:results_col3
        (represent_word (script_func max_date) f);
    | S_history (w, r, i) ->
      model#set ~row ~column:results_col2
        (represent_word (script_func i) w);
      model#set ~row ~column:results_col3
        (represent_rule (script_func i) r);
      model#set ~row ~column:results_col4
        (string_of_int i)
  in
  iter_model iter model

(* Toplevel classes *)

class preferences = object (self)

end

class virtual ['a] skel_view = object (self)

  initializer self#refresh

  val virtual widget : GTree.view
  val mutable data = None
  val mutable model = None
  val mutable font = ""

  method virtual private refresh : unit

  method data : 'a option = data

  method coerce = widget#coerce

  method event = widget#event

  method set_font s =
    font <- s;
    self#refresh

end

class virtual ['a] default_view = object (self)

  initializer self#refresh

  inherit ['a] skel_view

  val mutable file = ""
  val mutable script = ipa_script

  method file = file

  method clear_data =
    data <- None;
    model <- None;
    file <- "";
    widget#set_model None;
    self#refresh

  method set_script s =
    let old = script in
    try
      script <- s;
      self#refresh
    with exn ->
      script <- old;
      self#refresh;
      general_error_box exn

  method script = script

  method private fun_data lexing parsing from_data s f =
    let dummy_lexbuf = Lexing.from_string "" in
    let old_data = data
    and old_model = model
    and old_script = script
    and old_file = file
    in
    try
      let chan = open_in f in
      let ans = parsing
        (lexing s (Ulexing.from_utf8_channel chan)) dummy_lexbuf
      in
      close_in chan;
      data <- Some ans;
      model <- may from_data data;
      file <- f;
      script <- s;
      widget#set_model (may (fun x -> x#coerce) model);
      widget#expand_all ();
      self#refresh
    with exn ->
      let line =
        dummy_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
      in
      data <- old_data;
      model <- old_model;
      script <- old_script;
      file <- old_file;
      widget#set_model (may (fun x -> x#coerce) model);
      self#refresh;
      lexing_error_box (Filename.basename f) line exn

end

class lexicon_view = object (self)

  inherit [lexicon] default_view

  val widget =
    let w = GTree.view
      ~headers_visible:false
      ~rules_hint:true ()
    in
    w#append_column (GTree.view_column ());
    w

  method private refresh =
    let col = widget#get_column 0 in
    let cell = GTree.cell_renderer_text [`FONT font] in
    may (set_lexicon_script script) model;
    col#clear ();
    col#pack cell;
    col#add_attribute cell "text" lexicon_col2

  method set_data = self#fun_data
    Word_lexer.main Word_parser.parse from_lexicon

end

class ruleset_view = object (self)

  inherit [rule_set] default_view

  val widget =
    let w = GTree.view
      ~headers_visible:false
      ~rules_hint:true ()
    in
    w#append_column (GTree.view_column ());
    w#append_column (GTree.view_column ());
    w

  method private refresh =
    let col1 = widget#get_column 0
    and col2 = widget#get_column 1 in
    let cell = GTree.cell_renderer_text [`FONT font] in
    may (set_ruleset_script script) model;
    let func (model: GTree.model) row =
      let style = model#get ~row ~column:ruleset_col1
      and text = model#get ~row ~column:ruleset_col2
      in
      let default = [`TEXT text; `WEIGHT `NORMAL] in
      let extra = match style with
      | S_group _ -> [`WEIGHT `BOLD]
      | _ -> []
      in
      cell#set_properties (default @ extra)
    in
    col1#clear ();
    col1#pack cell;
    col2#clear ();
    col2#pack cell;
    col1#set_cell_data_func cell func;
    col2#add_attribute cell "text" ruleset_col3

  method set_data =
    self#fun_data Rule_lexer.main Rule_parser.parse from_ruleset

end

class results_view = object (self)

  inherit [result list] skel_view

  val widget =
    let w = GTree.view
      ~headers_visible:false
      ~rules_hint:true ()
    in
    w#append_column (GTree.view_column ());
    w#append_column (GTree.view_column ());
    w#append_column (GTree.view_column ());
    w

  val mutable show_date = false

  val mutable script_fun = (ipa_script, [])

  val mutable prop = (None, None, None)

  method set_script s =
    let old = script_fun in
    try
      script_fun <- (s, []);
      self#refresh
    with exn ->
      script_fun <- old;
      self#refresh;
      general_error_box exn

  method set_script_fun f =
    let old = script_fun in
    try
      script_fun <- f;
      self#refresh
    with exn ->
      script_fun <- old;
      self#refresh;
      general_error_box exn

  method script_fun = script_fun

  method properties = prop

  method private refresh =
    let col1 = widget#get_column 0
    and col2 = widget#get_column 1
    and col3 = widget#get_column 2 in
    let cell = GTree.cell_renderer_text [`FONT font] in
    let (min_date, max_date) = match prop with
    | (None, None, _) -> min_int, max_int
    | (None, Some x, _) -> min_int, x
    | (Some x, None, _) -> x, max_int
    | (Some x, Some y, _) -> x, y
    in
    let (init, list) = script_fun in
    let script i =
      let rec aux accu = function
      | [] -> accu
      | (date, script) :: q ->
        if i < date then accu
        else aux script q
      in
      aux init list
    in
    may (set_results_script script min_date max_date) model;
    col1#clear ();
    col1#pack cell;
    col2#clear ();
    col2#pack cell;
    col3#clear ();
    col3#pack cell;
    col1#add_attribute cell "text" results_col2;
    col2#add_attribute cell "text" results_col3;
    col3#add_attribute cell "text" results_col4;
    col3#set_visible show_date

  method clear_data =
    data <- None;
    model <- None;
    widget#set_model None;
    self#refresh

  method set_show_date bool =
    show_date <- bool;
    (widget#get_column 2)#set_visible show_date

  method toggle_show_date () =
    show_date <- not show_date;
    (widget#get_column 2)#set_visible show_date

  method set_data lexicon ruleset min_date max_date dialect =
    let old_data = data
    and old_prop = prop in
    let thread () =
    (
      let (s, d, p) = progress_box () in
      GtkThread.sync s ();
      let progress f = GtkThread.async p f in
      try
        let result = apply_set ~progress ruleset lexicon
          min_date max_date dialect ()
        in
        GtkThread.sync d ();
        data <- Some result;
        model <- may from_results data;
        widget#set_model (may (fun x -> x#coerce) model);
        prop <- (min_date, max_date, dialect);
        self#refresh;
      with exn ->
        GtkThread.sync d ();
        data <- old_data;
        prop <- old_prop;
        self#refresh;
        general_error_box exn
    )
    in
    Thread.create thread ();
    ()

end

(* Script container *)

class script_list = object (self)

  val mutable scripts = []

  method clear = scripts <- []

  method get = ipa_script :: scripts

  method add f =
    let dummy_lexbuf = Lexing.from_string "" in
    let name = Filename.basename f in
    try
      let chan = open_in f in
      let lexbuf = Ulexing.from_utf8_channel chan in
      let ans = Converter_parser.parse
        (Converter_lexer.main name lexbuf) dummy_lexbuf
      in
      close_in chan;
      if name = "IPA" || (List.exists (fun x -> x#name = name) scripts) then
        failwith "add"
      else
        scripts <- ans :: scripts
    with exn ->
      let line =
        dummy_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
      in
      lexing_error_box name line exn

end
