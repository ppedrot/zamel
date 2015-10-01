type document = Dom_html.document Js.t

class widget : document -> #Dom_html.element Js.t ->
object
  method as_node : Dom.node Js.t
  method as_element : Dom_html.element Js.t
  method show : unit -> unit
  method hide : unit -> unit
end

class label : document -> ?label:string -> unit ->
object
  inherit widget
  method label : string
  method set_label : string -> unit
end

type 'a elt =
| Node of 'a * 'a elt list

type path =
| Here
| Down of path
| Step of path

type _ descr =
| Nil : unit descr
| Elt : 'a descr -> ('a * (#Dom_html.element Js.t as 'b)) descr

exception Invalid_path

class ['a] tree : document -> 'a descr -> unit ->
object
  inherit widget
  method insert : path -> 'a -> unit
  method set : path -> 'a -> unit
end
