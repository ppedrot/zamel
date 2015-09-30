module Html = Dom_html

(** Payload for the javascript document *)

let onload _  =
  let doc = Html.document in
  let body = doc##getElementById (Js.string "zamel") in
  let body = Js.Opt.case body (fun () -> assert false) (fun x -> x) in
  let div = Html.createDiv doc in
  let display = Html.createP doc in
  let button = Html.createButton doc in
  let () = button##innerHTML <- (Js.string "Process") in
  let () = Dom.appendChild body button in
  let () = Dom.appendChild body div in
  let () = Dom.appendChild div display in
  let () = div##style##textAlign <- (Js.string "center") in
  let () = div##style##fontSize <- (Js.string "200%") in
  let () = div##style##fontFamily <- (Js.string "palatino, serif") in
  let () = div##style##fontStyle <- (Js.string "italic") in
  let onclick _ =
    let () = display##innerHTML <- (Js.string "foo") in
    Js._true
  in
  let () = button##onclick <- Html.handler onclick in
  Js._false

let () = Html.window##onload <- Html.handler onload
