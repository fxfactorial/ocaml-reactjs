
type class_opts = {display_name: string; }

type element_opts = {class_name: string; }

(* type component_lifecycle =  *)

(* let create_element element_opts = () *)

let create_element class_ =
  Reactjs.react##createElement_WithReactClass class_ Js.null

let render element dom_elem =
  Reactjs.reactDOM##render element dom_elem

let create_class class_opts = ()
