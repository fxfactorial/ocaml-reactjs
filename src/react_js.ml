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
    ~render_f:(render_f : (unit -> Js.Unsafe.any))
    ~display_name:(display_name : string)
    () = object(self)

  val raw_js = object%js

    val displayName = Js.string display_name
    val propTypes = match prop_types with
        None -> Js.null | Some p -> !!p |> Js.Opt.return
    val render =  !! !@(fun () ->
        render_f ()
      )

  end

  method unsafe_raw = raw_js

end

and react_class (elem : Js.Unsafe.any) = object

  method unsafe_raw = elem

end

and react_element (elem : Js.Unsafe.any) = object

  method unsafe_raw = elem

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
  type a. a elem_arg -> Js.Unsafe.any = function
  (* type a. a elem_arg -> react_element option = function *)
  | React_class c ->
    (* new react_element (react##createElement c#unsafe_raw Js.null) *)
    react##createElement c#unsafe_raw Js.null
  | New_elem {html_elem_t; class_name; content;} ->
    let elem =
      react##createElement
        (Js.string html_elem_t)
        (object%js
          val className = Js.string class_name
        end)
        (Js.string content)
    in
    elem
    (* new react_element elem *)

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
    ~react_elem:(react_elem : Js.Unsafe.any)
    (elem : #Dom_html.element Js.t) =
    match cb with
    | None ->
      stringify react_elem |> print_endline;
      react_dom##render react_elem elem
    | Some f ->
      react_dom##render react_elem elem !@f

end

let () =
  let commentbox =
    new component_spec
      ~display_name:"CommentBox"
      ~render_f:(fun () ->
          create_element
            (New_elem {html_elem_t = "div";
                       class_name = "commentBox";
                       content = "Hello world, I'm a commentbox"})
        )
      ()
    |> create_class
  in
  let comment_box_instance = create_element (React_class commentbox) in
  ignore (
    ReactDOM.render
      ~react_elem:comment_box_instance
      (Dom_html.getElementById "content")
  )
