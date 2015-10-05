module Html = Dom_html

type document = Html.document Js.t

class widget (doc : document) (obj : #Html.element Js.t) =
object
  val document = doc
  method as_node = (obj :> Dom.node Js.t)
  method as_element = (obj :> Html.element Js.t)
  method show () = obj##classList##remove (Js.string "mlui-hidden")
  method hide () = obj##classList##add (Js.string "mlui-hidden")
end

class label doc ?label () =
let obj = Html.createP doc in
object (self)
  inherit widget doc obj
  method label = Js.to_string (obj##innerHTML)
  method set_label s = obj##innerHTML <- Js.string s

  initializer
    begin match label with None -> () | Some s -> self#set_label s end;
    obj##className <- Js.string "mlui-label";

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

class ['a] row doc (descr : 'a descr) cells =
let row = Html.createTr doc in
let handle = Html.createTd doc in
let anchor = Html.createA doc in
object
  inherit widget doc row

  method handle = new widget doc anchor

  initializer
    let cells = to_list descr cells in
    let iter elt =
      let cell = Html.createTd doc in
      Dom.appendChild cell elt;
      Dom.appendChild row cell;
    in
    anchor##href <- Js.string "#";
    anchor##innerHTML <- Js.string "▸";
    anchor##className <- Js.string "mlui-tree-anchor";
    Dom.appendChild handle anchor;
    handle##style##paddingRight <- Js.string "1em";
    Dom.appendChild row handle;
    List.iter iter (List.rev cells);
    row##className <- Js.string "mlui-tree-row";
end

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
      let row = new row doc descr v in
      row#handle#as_element##onclick <- Html.handler (fun _ -> row#hide (); Js._false);
      Dom.insertBefore obj row#as_node pos
    in
    iter path f (obj##firstChild)

  method set path (v : 'a) =
    let up = function
    | [] -> raise Invalid_path
    | Node (_, data) :: rem -> Node (v, data) :: rem
    in
    cols <- update path up cols;
    let f pos =
      let row = new row doc descr v in
      let pos = Js.Opt.get pos (fun () -> assert false) in
      Dom.replaceChild obj row#as_node pos
    in
    iter path f (obj##firstChild)

  initializer
    obj##className <- Js.string "mlui-tree";

end

class notebook doc () =
let obj = Html.createDiv doc in
let bar = Html.createDiv doc in
let box = Html.createDiv doc in
object (self)
  val mutable id = 0
  val mutable panels : (int * Html.element Js.t) list = []
  val mutable labels : (int * Html.anchorElement Js.t) list = []
  inherit widget doc obj

  method private select_id id =
    let iter (uid, obj) =
      let cl = Js.string "mlui-notebook-tab-hidden" in
      if id = uid then obj##classList##remove (cl)
      else obj##classList##add (cl)
    in
    List.iter iter panels;
    let iter (uid, obj) =
      let cl = Js.string "mlui-notebook-label-selected" in
      if id = uid then obj##classList##add (cl)
      else obj##classList##remove (cl)
    in
    List.iter iter labels;

  method insert ?(label = "") widget =
    let anchor = Html.createA doc in
    let nid = id + 1 in
    id <- nid;
    anchor##innerHTML <- (Js.string label);
    anchor##href <- (Js.string "#");
    panels <- (nid, widget) :: panels;
    labels <- (nid, anchor) :: labels;
    let onclick _ = self#select_id nid; Js._false in
    anchor##onclick <- Html.handler onclick;
    anchor##className <- Js.string "mlui-notebook-label";
    Dom.appendChild box widget;
    Dom.appendChild bar anchor;
    self#select_id 1

  initializer
    obj##className <- Js.string "mlui-notebook";
    Dom.appendChild obj bar;
    Dom.appendChild obj box;

end
