module Html = Dom_html

open Widget

let () = Ipadata.init "/static/ipadata.xml"

(** Payload for the javascript document *)

class textView (doc : Html.document Js.t) =
let obj = Html.createTextarea doc in
object
  inherit Widget.widget doc obj
  val obj = obj

  initializer
    obj##cols <- 80;
    obj##rows <- 15;
    obj##setAttribute (Js.string "spellcheck", Js.string "false")
end

class lexicon (doc : Html.document Js.t) =
object
  inherit textView doc

  initializer
    obj##value <- Js.string (Sys_js.file_content "/static/latin.lex")

  val mutable data = []

  method data = data

  method parse () =
    let text = Js.to_string obj##value in
    let dummy_lexbuf = Lexing.from_string "" in
    try
      let script = Converter.ipa_script in
      let buf = Ulexing.from_utf8_string text in
      let lexing = Word_lexer.main script buf in
      let ans = Word_parser.parse lexing dummy_lexbuf in
      data <- ans
    with exn ->
      ()
(*       let line = dummy_lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum in *)
(*       lexing_error_box (Filename.basename f) line exn *)

end

class ruleset (doc : Html.document Js.t) =
object
  inherit textView doc
  val mutable data = Data_set.create_ruleset [] [] [] []

  initializer
    obj##value <- Js.string (Sys_js.file_content "/static/ancient_french.sc")

  method data = data

  method parse () =
    let text = Js.to_string obj##value in
    let dummy_lexbuf = Lexing.from_string "" in
    try
      let script = Converter.ipa_script in
      let buf = Ulexing.from_utf8_string text in
      let lexing = Rule_lexer.main script buf in
      let ans = Rule_parser.parse lexing dummy_lexbuf in
      data <- ans
    with exn ->
      ()

end

class results (doc : Html.document Js.t) =
let obj = Html.createDiv doc in
object
  inherit Widget.widget doc obj

  method compute lexicon ruleset =
    let result = Data_set.apply_set ruleset lexicon None None None () in
    let of_binary word = Data_set.represent_word Converter.ipa_script word in
    let descr = Elt (Elt Nil) in
    let table = new tree doc descr () in
    let () = Dom.appendChild obj table#as_node in
    let iter ans =
      let src = new label doc ~label:(of_binary ans.Data_set.original) () in
      let dst = match ans.Data_set.history with
      | [] -> new label doc ()
      | (word, _, _) :: _ -> new label doc ~label:(of_binary word) ()
      in
      table#insert Here (((), src#as_element), dst#as_element)
(*      let steps (word, rule, date) =
        let tr = Html.createTr doc in
        let label = new label doc ~label:(of_binary word) () in
        Dom.appendChild tr label#as_node;
        tr
      in
      (src, dst, List.rev_map steps ans.Data_set.history)*)
    in
    List.iter iter (List.rev result);
    Js.Opt.case (obj##firstChild)
      (fun () -> Dom.appendChild obj table#as_node)
      (fun child -> Dom.replaceChild obj table#as_node child)

end

let onload _  =
  let doc = Html.document in
  let body = doc##getElementById (Js.string "zamel") in
  let body = Js.Opt.case body (fun () -> assert false) (fun x -> x) in
  let button = Html.createButton doc in
  let lexicon = new lexicon doc in
  let ruleset = new ruleset doc in
  let results = new results doc in
  let notetab = new notebook doc () in
  let () = notetab#insert ~label:"Lexicon" lexicon#as_element in
  let () = notetab#insert ~label:"Rules" ruleset#as_element in
  let () = notetab#insert ~label:"Results" results#as_element in
  let () = button##innerHTML <- (Js.string "Process") in
  let () = Dom.appendChild body notetab#as_node in
  let () = Dom.appendChild body button in
  let onclick _ =
    let () = lexicon#parse () in
    let () = ruleset#parse () in
    let () = results#compute lexicon#data ruleset#data in
    Js._true
  in
  let () = button##onclick <- Html.handler onclick in
  Js._false

let () = Html.window##onload <- Html.handler onload
