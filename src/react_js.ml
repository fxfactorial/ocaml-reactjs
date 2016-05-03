let ( <!> ) obj field = Js.Unsafe.get obj field
let ( !@ ) f = Js.wrap_callback f
let ( !! ) o = Js.Unsafe.inject o
let stringify o = Js._JSON##stringify o |> Js.to_string
let m = Js.Unsafe.meth_call

let (react, react_dom) =
  Js.Unsafe.global##.React,
  Js.Unsafe.global##.ReactDOM

let version = react <!> "version" |> Js.to_string

class component_spec
    ?(get_initial_state :(unit -> Js.Unsafe.any) option)
    ?(get_default_props : (unit -> Js.Unsafe.any) option)
    ?(prop_types : Js.Unsafe.any option)
    ?(mixins : Js.Unsafe.any Js.js_array option)
    ?(statics : Js.Unsafe.any option)
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
    ~render_f:(render_f : (unit -> react_element))
    ~display_name:(display_name : string)
    () = object(self)

  val raw_js = object%js

    val displayName = Js.string display_name
    val propTypes = match prop_types with
        None -> Js.null | Some p -> !!p |> Js.Opt.return
    val render =  !! !@(fun () ->
        (render_f ())#unsafe_raw
      )

  end

  method unsafe_raw = raw_js

end

and react_class (wrapped_elem : Js.Unsafe.any) = object

  method unsafe_raw = wrapped_elem

end

and react_element (wrapped_elem : Js.Unsafe.any) = object

  method unsafe_raw = wrapped_elem

end

let create_class (com_spec : component_spec) =
  new react_class (react##createClass com_spec#unsafe_raw)

type _ elem_arg =
  | React_class : react_class -> react_class elem_arg
  | New_elem : args -> unit elem_arg
and args = {html_elem_t : string;
            class_name : string;
            content : string;}

let create_element :
  type a. a elem_arg -> react_element = function
  | React_class c ->
    (react##createElement c#unsafe_raw Js.null)
    |> new react_element
  | New_elem {html_elem_t; class_name; content;} ->
    react##createElement
      (Js.string html_elem_t)
      (object%js
        val className = Js.string class_name
      end)
      (Js.string content)
    |> new react_element

let clone_element ?children ?props (elem : react_element) =
  ()

let create_factory
    (elem : [`Html_elem of string |
             `React_class of react_class]) =
  ()

let is_valid_element obj = ()

module DOM = struct end

module PropTypes = struct end

module Children = struct end

module ReactDOM = struct

  let render
      ?on_update_or_render:(cb : (unit -> unit) option)
      ~react_elem:(react_elem : react_element)
      (elem : #Dom_html.element Js.t) = Js.Opt.(
      let result = match cb with
        | None ->
          react_dom##render react_elem#unsafe_raw elem
        | Some f ->
          react_dom##render react_elem#unsafe_raw elem !@f
      in
      if Js.Opt.test result
      then return (new react_element result) |> to_option
      else Js.null |> to_option
    )

end

(* let () = *)
(*   let commentbox = *)
(*     new component_spec *)
(*       ~display_name:"CommentBox" *)
(*       ~render_f:(fun () -> *)
(*           create_element *)
(*             (New_elem {html_elem_t = "div"; *)
(*                        class_name = "commentBox"; *)
(*                        content = "Hello world, I'm a comment box"}) *)
(*         ) *)
(*       () *)
(*     |> create_class *)
(*   in *)
(*   let comment_box_instance = *)
(*     create_element (React_class commentbox) *)
(*   in *)
(*   match *)
(*     ReactDOM.render *)
(*       ~react_elem:comment_box_instance *)
(*       ~on_update_or_render:(fun () -> print_endline "Called Render") *)
(*       (Dom_html.getElementById "content") *)
(*   with *)
(*   | None -> print_endline "Couldn't render element" *)
(*   | Some _ -> print_endline "Rendered!" *)
