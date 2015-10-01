module Html = Dom_html

type document = Html.document Js.t

class widget (doc : document) (obj : #Html.element Js.t) =
object
  val document = doc
  method as_node = (obj :> Dom.node Js.t)
  method as_element = (obj :> Html.element Js.t)
  method show () = obj##style##display <- Js.string "auto"
  method hide () = obj##style##display <- Js.string "none"
end

class label doc ?label () =
let obj = Html.createP doc in
object (self)
  inherit widget doc obj
  method label = Js.to_string (obj##innerHTML)
  method set_label s = obj##innerHTML <- Js.string s

  initializer
    match label with None -> () | Some s -> self#set_label s
end

type 'a elt =
| Node of 'a * 'a elt list

type path =
| Here
| Down of path
| Step of path

exception Invalid_path

let rec update path f data = match path with
| Here -> f data
| Down path ->
  begin match data with
  | [] -> raise Invalid_path
  | head :: data -> head :: update path f data
  end
| Step path ->
  begin match data with
  | [] -> raise Invalid_path
  | Node (w, data) :: rem -> Node (w, update path f data) :: rem
  end

let rec iter path f (data : Dom.node Js.t Js.opt) = match path with
| Here -> f data
| Down path ->
  let data = Js.Opt.get data (fun () -> assert false) in
  iter path f (data##nextSibling)
| Step path -> iter path f data

type _ descr =
| Nil : unit descr
| Elt : 'a descr -> ('a * (#Html.element Js.t as 'b)) descr

let rec to_list : type a. a descr -> a -> Html.element Js.t list = function
| Nil -> fun () -> []
| Elt descr -> fun (row, e) -> (e :> Html.element Js.t) :: to_list descr row

let as_row doc descr cells =
  let cells = to_list descr cells in
  let row = Html.createTr doc in
  let iter elt =
    let cell = Html.createTd doc in
    Dom.appendChild cell elt;
    Dom.appendChild row cell;
  in
  List.iter iter cells;
  row

(* "▶" *)

class ['a] tree doc (descr : 'a descr) () =
let obj = Html.createTable doc in
let () = obj##setAttribute (Js.string "style", Js.string "margin:auto;") in
object (self)
  inherit widget doc obj
  val mutable cols : 'a elt list = []

  method insert path (v : 'a) =
    let up data = Node (v, []) :: data in
    cols <- update path up cols;
    let f pos =
      let row = as_row doc descr v in
      Dom.insertBefore obj row pos
    in
    iter path f (obj##firstChild)

  method set path (v : 'a) =
    let up = function
    | [] -> raise Invalid_path
    | Node (_, data) :: rem -> Node (v, data) :: rem
    in
    cols <- update path up cols;
    let f pos =
      let row = as_row doc descr v in
      let pos = Js.Opt.get pos (fun () -> assert false) in
      Dom.replaceChild obj row pos
    in
    iter path f (obj##firstChild)


end
