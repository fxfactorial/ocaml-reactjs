open StdLabels

type 'a javascript_object = 'a Js.t

type 'a component_api = (<isMounted : bool Js.t Js.meth; .. > as 'a) Js.t

module Helpers = struct
  let set_interval ~f ~every =
    Dom_html.window##setInterval (Js.wrap_callback f) every
  let get_elem ~id = Dom_html.getElementById id

end

include Helpers

module Infix = struct

  let ( !@ ) = Js.wrap_callback

  let ( !^ ) = Js.Unsafe.inject

  let ( <!> ) (obj : 'a Js.t) field = Js.Unsafe.get obj (Js.string field)

  let ( <?> ) (obj : 'a Js.t) field = Js.Opt.(obj <!> field |> return |> to_option)

  let ( <@> ) (obj : 'a Js.t) field = Js.Optdef.(obj <!> field |> return |> to_option)

  (* merge *)
  let ( <+> ) (a :'a Js.t) (b : 'b Js.t) : < .. > Js.t = Js.Unsafe.global##.Object##assign a b

  let ( !$ ) (o : 'a Js.t) = Js._JSON##stringify o |> Js.to_string

  let ( !* ) = Js.string

  let ( *! ) = Js.to_string

  let ( $> ) g =
    g |> Js.str_array |> Js.to_array |> Array.map ~f:Js.to_string |> Array.to_list

  let ( <$ ) g = g |> Array.of_list |> Array.map ~f:Js.string |> Js.array

end

module Low_level_bindings = struct

  let (__react, __reactDOM, __reactDOMServer) :
    'a Js.t * 'a Js.t * 'a Js.t =
    let open Js.Unsafe in
    let undef = Js.Unsafe.eval_string "undefined" in
    let require_module s =
      fun_call (js_expr "require") [|inject (Js.string s)|]
    in
    try
      (* Need to keep it this way, otherwise jsoo will optimize it
         out, also add this to js_of_ocaml *)
      Js.typeof (eval_string "window") = Js.string "undefined";
      (* In Browser *)
      global##.React,
      global##.ReactDOM,
      undef
    with Js.Error _ ->
      (* In Node *)
      (try require_module "react" with _ -> undef),
      (try require_module "react-dom" with _ -> undef),
      (try require_module "react-dom-server" with _ -> undef)

  class type react_dom_server = object
    method renderToString :
      react_element Js.t -> Js.js_string Js.t Js.meth
    method renderToStaticMarkup :
      react_element Js.t -> Js.js_string Js.t Js.meth
  end

  and react_dom = object
    method render :
      react_element Js.t -> #Dom_html.element Js.t -> unit Js.meth
    (* method render_WithCallback : *)
    (*   react_element Js.t -> #Dom_html.element Js.t -> unit Js.meth *)

    method unmountComponentAtNode :
      #Dom_html.element Js.t -> bool Js.t Js.meth

    (* method findDOMNode : *)
    (*    Js.t -> #Dom_html.element Js.t Js.meth *)

  end

  and ['this] react = object

    constraint 'this = _ component_api

    method createElement_withString :
      Js.js_string Js.t -> react_element Js.t Js.meth

    method createElement_withPropsAndSingleText :
      Js.js_string Js.t ->
      <className: Js.js_string Js.t Js.readonly_prop> Js.t ->
      Js.js_string Js.t ->
      react_element Js.t Js.meth

    method createElement_WithReactClass :
      react_class Js.t -> _ Js.Opt.t -> react_element Js.t Js.meth
    method cloneElement : react_element Js.t -> react_element Js.t Js.meth
    method isValidElement : 'a Js.t -> bool Js.t Js.meth

    method createClass :
      <
        render :
          ('this, react_element Js.t) Js.meth_callback Js.readonly_prop;
        getInitialState :
          ('this, 'b Js.t) Js.meth_callback Js.Optdef.t Js.readonly_prop;
        getDefaultProps :
          ('this, 'default_props Js.t ) Js.meth_callback Js.Optdef.t Js.readonly_prop;
        propTypes : 'props_validator Js.t Js.Optdef.t Js.readonly_prop;
        mixins : 'mixin Js.t Js.js_array Js.t Js.Optdef.t Js.readonly_prop;
        statics : 'static_functions Js.t Js.Optdef.t Js.readonly_prop;
        displayName : Js.js_string Js.t Js.Optdef.t Js.readonly_prop;
        (* Lifecycle Methods *)
        componentWillMount :
          ('this, unit) Js.meth_callback Js.Optdef.t Js.readonly_prop;
        componentDidMount :
          ('this, unit) Js.meth_callback Js.Optdef.t Js.readonly_prop;
        componentWillReceiveProps :
          ('this,
           'next_props Js.t -> unit) Js.meth_callback Js.Optdef.t Js.readonly_prop;
        shouldComponentUpdate :
          ('this, 'next_props Js.t -> 'next_state Js.t -> bool Js.t)
            Js.meth_callback Js.Optdef.t Js.readonly_prop;
        componentWillUpdate :
          ('this, 'next_prop Js.t -> 'next_state Js.t -> unit)
            Js.meth_callback  Js.Optdef.t Js.readonly_prop;
        componentDidUpdate :
          ('this,
           'prev_prop Js.t -> 'prev_state Js.t -> unit)
            Js.meth_callback Js.Optdef.t Js.readonly_prop;
        componentWillUnmount :
          ('this, unit)
            Js.meth_callback Js.Optdef.t Js.readonly_prop;
      > Js.t ->
      react_class Js.t Js.meth

    method createFactory :
      react_class Js.t -> (prop:'new_prop Js.t -> react_element Js.t) Js.meth

    method version : Js.js_string Js.t Js.readonly_prop
    (* method __spread *)
    method _DOM : 'a Js.t Js.readonly_prop
  end

  and react_element = object

    method type_ : Js.js_string Js.t Js.readonly_prop
    method key : 'a Js.t Js.Opt.t Js.prop
    (* method ref : react_element_ref Js.t Js.Opt.t Js.prop *)

  end

  and react_class = object

  end

  let react :
    'this .
      (< isMounted : bool Js.t Js.meth; .. > as 'this) component_api react Js.t
    = __react

  let reactDOM : react_dom Js.t = __reactDOM

  (* Only makes sense on the server, hence the unit *)
  let reactDOMServer : unit -> react_dom_server Js.t = fun () -> __reactDOMServer

end

let debug thing field =
  Firebug.console##log
    (Js.Unsafe.(meth_call (get thing field) "toString" [||]))

type element_spec = { class_name: string option; } [@@deriving make]

(* type children = [`text of string list *)
(*                 | `kids of Low_level_bindings.react_element Js.t list ] *)

(* think React_fragment as well? *)
type _ react_node =
  | Text : string -> string react_node
  | React_element :
      Low_level_bindings.react_element Js.t ->
    Low_level_bindings.react_element Js.t react_node

type ('this,
      'initial_state,
      'default_props,
      'prop_types,
      'static_functions,
      'next_props,
      'next_state,
      'prev_props,
      'prev_state,
      'props,
      'mixin) class_spec =
  { render:
      this:'this component_api ->
      Low_level_bindings.react_element Js.t; [@main]
    initial_state : (this:'this component_api -> 'initial_state Js.t) option;
    default_props : (this:'this component_api -> 'default_props Js.t) option;
    prop_types : 'prop_types Js.t option;
    mixins : 'mixin Js.t list option;
    statics : 'static_functions Js.t option;
    display_name : string option;
    component_will_mount : (this:'this Js.t -> unit) option;
    component_did_mount : (this:'this Js.t -> unit) option;
    component_will_receive_props :
      (this:'this Js.t -> next_prop:'next_props Js.t -> unit) option;
    should_component_update :
      (this:'this Js.t ->
       next_prop:'next_props Js.t ->
       next_state:'next_state Js.t -> bool Js.t) option;
    component_will_update :
      (this:'this Js.t ->
       next_prop:'next_props Js.t ->
       next_state:'next_state Js.t -> unit) option;
    component_did_update :
      (this:'this Js.t ->
       prev_prop:'prev_props Js.t ->
       prev_state:'prev_state Js.t -> unit) option;
    component_will_unmount : (this:'this Js.t -> unit) option;
  } [@@deriving make]

let create_element ?element_opts elem_name children = Js.Unsafe.(
    let g = children |> List.map ~f:(function
        | Text s -> inject (Js.string s)
        | React_element e -> inject e
      )
    in
    [
      [|
        inject (Js.string elem_name);
        match element_opts with
          None -> Js.null |> inject
        | Some spec ->
          inject (object%js(self)
            val className =
              Js.Opt.(map (option spec.class_name) Js.string)
          end)
      |];
      Array.of_list g
    ]
    |> Array.concat
    |> Js.Unsafe.meth_call Low_level_bindings.__react "createElement"
  )

let create_element_from_class class_ =
  Low_level_bindings.react##createElement_WithReactClass class_ Js.null

let create_factory :
  Low_level_bindings.react_class Js.t ->
  (props:'a Js.t -> Low_level_bindings.react_element Js.t) = fun c ->
  let call_me = Low_level_bindings.react##createFactory c in
  fun ~props -> Js.Unsafe.fun_call call_me [|Js.Unsafe.inject props|]

let create_class class_opts = let open Js.Optdef in
  let comp = (object%js
    (* Component Specifications *)
    val render = Js.wrap_meth_callback (fun this -> class_opts.render ~this)
    val getInitialState =
      (fun f -> Js.wrap_meth_callback (fun this -> f ~this))
      |> map (option class_opts.initial_state)
    val getDefaultProps =
      (fun f -> Js.wrap_meth_callback (fun this -> f ~this))
      |> map (option class_opts.default_props)
    val propTypes = option class_opts.prop_types
    val mixins = map (option class_opts.mixins) (fun m -> Array.of_list m |> Js.array)
    val statics = option class_opts.statics
    val displayName = map (option class_opts.display_name) Js.string
    (* Lifecycle Methods *)
    val componentWillMount =
      (fun f -> Js.wrap_meth_callback (fun this -> f ~this))
      |> map (option class_opts.component_will_mount)
    val componentDidMount =
      (fun f -> Js.wrap_meth_callback (fun this -> f ~this))
      |> map (option class_opts.component_did_mount)
    val componentWillReceiveProps =
      (fun f ->  Js.wrap_meth_callback (fun this next_prop -> f ~this ~next_prop))
      |> map (option class_opts.component_will_receive_props)
    val shouldComponentUpdate =
      (fun f -> Js.wrap_meth_callback
          (fun this next_prop next_state -> f ~this ~next_prop ~next_state))
      |> map (option class_opts.should_component_update)
    val componentWillUpdate =
      (fun f -> Js.wrap_meth_callback
          (fun this next_prop next_state -> f ~this ~next_prop ~next_state))
      |> map (option class_opts.component_will_update)
    val componentDidUpdate =
      (fun f ->  Js.wrap_meth_callback
          (fun this prev_prop prev_state -> f ~this ~prev_prop ~prev_state))
      |> map (option class_opts.component_did_update)
    val componentWillUnmount =
      (fun f -> Js.wrap_meth_callback (fun this -> f ~this))
      |> map (option class_opts.component_will_unmount)
  end)
  in
  Low_level_bindings.react##createClass comp

let elem_from_spec spec = create_element_from_class (create_class spec)

let render ~react_elem dom_elem =
  Low_level_bindings.reactDOM##render react_elem dom_elem

module DOM = struct

  type tag = [`abbr | `address | `area | `article | `aside | `audio |
              `b | `base | `bdi | `bdo | `big | `blockquote | `body |
              `br | `button | `canvas | `caption | `cite | `code |
              `col | `colgroup | `data | `datalist | `dd | `del |
              `details | `dfn | `dialog | `div | `dl | `dt | `em |
              `emded | `fieldset | `figcaption | `figure | `footer |
              `form | `h1 | `h2 | `h3 | `h5 | `h6 | `head | `header |
              `hgroup | `hr | `html | `i | `iframe | `img | `input |
              `ins | `kbd | `keygen | `label | `legend | `li | `link |
              `main | `map | `mark | `menu | `menuitem | `meta | `meter |
              `nav | `noscript |
              `object_ [@printer fun fmt -> fprintf fmt "object"] |
              `ol | `optgroup | `option | `output | `p | `param | `picture |
              `pre | `progress | `q | `rp | `rt | `ruby | `s | `samp |
              `script | `section | `select | `small | `source | `span |
              `strong | `style | `sub | `summary | `sup | `table |
              `tbody | `td | `textarea | `tfoot | `th | `thead |
              `time | `title | `tr | `track | `u | `ul | `var | `video |
              `wbr | `circle | `clipPath | `defs | `ellipse | `g |
              `image | `line | `linearGradient | `mask | `path |
              `pattern | `polygon | `polyline | `radialGradient |
              `rect | `stop | `svg | `text | `tspan ] [@@deriving show]

  let string_of_tag tag =
    (tag |> Js.string)##substring_toEnd 1 |> Js.to_string

  type 'a elem_spec =
    (<className: Js.js_string Js.t Js.readonly_prop; .. > as 'a ) Js.t

  (* let make *)
  (*     ?(elem_spec : 'a javascript_object option) *)
  (*     ~tag *)
  (*     (c : children) : Low_level_bindings.react_element Js.t = *)
  (*   let elem_name = show_tag tag |> string_of_tag in *)
  (*   let args = match c with *)
  (*     | `text s -> *)
  (*       Js.Unsafe.inject Js.null :: *)
  (*       (List.map (fun s -> Js.string s |> Js.Opt.return |> Js.Unsafe.inject) s) *)
  (*     | _ -> [] *)
  (*   in *)
  (*   Js.Unsafe.meth_call *)
  (*     Low_level_bindings.react##._DOM *)
  (*     elem_name *)
  (*     (Array.of_list (Js.Unsafe.inject (Js.Opt.option elem_spec) :: args)) *)

end
