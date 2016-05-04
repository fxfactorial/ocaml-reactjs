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
    ~render_f:(render_f : (react_element -> react_element))
    ~display_name:(display_name : string)
    () = object(self)

  val mutable raw_js = !!(object%js end)

  initializer
    self#_set_raw_js

  method private _set_raw_js =
    raw_js <- !!(object%js
    val displayName = Js.string display_name
    val propTypes = match prop_types with
        None -> Js.null | Some p -> !!p |> Js.Opt.return
    val render = !!(Js.wrap_meth_callback (fun this ->
        (new react_element this
         |> render_f)
        #unsafe_raw
      ))
      end)

  method unsafe_raw = raw_js

end

and react_class (wrapped_elem : Js.Unsafe.any) = object

  method unsafe_raw = wrapped_elem

end

and react_element (wrapped_elem : Js.Unsafe.any) = object

  val mutable props_ : Js.Unsafe.any =
    Js.Unsafe.get wrapped_elem "props"

  method unsafe_raw = wrapped_elem

  method props = props_

  method get key : Js.Unsafe.any =
    Js.Unsafe.get props_ (Js.string key)

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


let create_factory (class_ : react_class) :
  < .. > Js.t option -> react_element =
  let factory = react##createFactory class_#unsafe_raw in
  fun props ->
    (match props with
     | None -> Js.Unsafe.fun_call factory [||]
     | Some p -> Js.Unsafe.fun_call factory [|!!props|])
    |> new react_element

let is_valid_element obj = ()

module DOM = struct

  let handle = react##.DOM

  let a ?(props : < .. > Js.t option) msg =
    (handle##a
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let abbr ?(props : < .. > Js.t option) msg =
    (handle##abbr
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let body ?(props : < .. > Js.t option) msg =
    (handle##body
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let code ?(props : < .. > Js.t option) msg =
    (handle##code
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let h1 ?(props : < .. > Js.t option) msg =
    (handle##h1
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let h2 ?(props : < .. > Js.t option) msg =
    (handle##h2
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let img ?(props : < .. > Js.t option) msg =
    (handle##img
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let p ?(props : < .. > Js.t option) msg =
    (handle##p
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element

  let div ?(props : < .. > Js.t option) msg =
    (handle##div
       (match props with None -> Js.null | Some p -> p |> Js.Opt.return)
       (Js.string msg))
    |> new react_element



end

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

let () =
  let example_application =
    new component_spec
      ~display_name:"CommentBox"
      ~render_f:(fun self ->
          let elapsed : float =
            Js.math##round
              (Js.Unsafe.get self#props "elapsed" /. 100.0)
          in
          let seconds = elapsed /. 10.0 in
          let message =
            seconds
            |> Printf.sprintf
              "React has been successfully running for %f seconds."
          in
          DOM.p message
        )
      ()
    |> create_class
  in
  let example_app_factory = create_factory example_application in
  let start = (new%js Js.date_now)##getTime in
  Printf.sprintf "%f" start
  |> print_endline;
  let a =
    Dom_html.window##setInterval
      !@(fun () ->
          ReactDOM.render
            (example_app_factory (object%js
               val elapsed = new%js Js.date_now)##getTime -. start
             end)
        )
    50.0
  in
  ()
  (* match *)
  (*   ReactDOM.render *)
  (*     ~react_elem:(DOM.p "Hello world, in a p") *)
  (*     (\* ~react_elem:comment_box_instance *\) *)
  (*     ~on_update_or_render:(fun () -> print_endline "Called Render") *)
  (*     (Dom_html.getElementById "content") *)
  (* with *)
  (* | None -> print_endline "Couldn't render element" *)
  (* | Some _ -> print_endline "Rendered!" *)
