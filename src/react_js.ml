let ( <!> ) obj field = Js.Unsafe.get obj field
let ( !@ ) f = Js.wrap_callback f
let ( !! ) o = Js.Unsafe.inject o
let m = Js.Unsafe.meth_call

let react = Js.Unsafe.global##.React
let version = react <!> "version" |> Js.to_string

class component_spec
    ?(get_initial_state : (unit -> Js.Unsafe.any) option)
    ?(get_default_props : (unit -> Js.Unsafe.any) option)
    ?(prop_types : Js.Unsafe.any option)
    ?(mixins : Js.Unsafe.any Js.js_array option)
    ?(statics : Js.Unsafe.any option)
    ?(display_name : string option)
    ?(component_will_mount : (unit -> unit) option)
    ?(component_did_mount : (unit -> unit) option)
    ?(component_will_receive_props :
        (Js.Unsafe.any -> unit) option)
    ?(should_component_update :
        (next_props:Js.Unsafe.any ->
         next_state:Js.Unsafe.any -> bool) option)
    ?(component_will_update :
        (next_props:Js.Unsafe.any ->
         next_state:Js.Unsafe.any -> unit) option)
    ?(component_did_update :
        (prev_props:Js.Unsafe.any ->
         prev_state:Js.Unsafe.any -> unit) option)
    ?(component_will_unmount : (unit -> unit) option)
    (* Later do the right binding for ReactElement? *)
    (render_f : unit -> Js.Unsafe.any) = object

end

class react_class = object

end

class react_element = object end

let create_class (com_spec : component_spec) = ()

let create_element
    ?children
    ?props
    (elem : [`Html_elem of string |
             `React_class of react_class]) =
  ()

let clone_element ?children ?props (elem : react_element) =
  ()

let create_factory (elem : [`Html_elem of string |
                            `React_class of react_class]) =
  ()

let is_valid_element obj = ()

module DOM = struct end

module PropTypes = struct end

module Children = struct end

