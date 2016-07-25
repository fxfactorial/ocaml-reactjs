open StdLabels

module rec Types : sig

  type tag = [`a | `abbr | `address | `area | `article | `aside | `audio |
              `b | `base | `bdi | `bdo | `big | `blockquote | `body |
              `br | `button | `canvas | `caption | `cite | `code |
              `col | `colgroup | `data | `datalist | `dd | `del |
              `details | `dfn | `dialog | `div | `dl | `dt | `em |
              `emded | `fieldset | `figcaption | `figure | `footer |
              `form | `h1 | `h2 | `h3 | `h4 | `h5 | `h6 | `head | `header |
              `hgroup | `hr | `html | `i | `iframe | `img | `input |
              `ins | `kbd | `keygen | `label | `legend | `li | `link |
              `main | `map | `mark | `menu | `menuitem | `meta | `meter |
              `nav | `noscript |
              `object_ [@printer fun fmt -> fprintf fmt "`object"] |
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

  type attributes = [`class_name of string
                    | `id of string]

  type attr_t = [`s of string | `i of int]

  type _ react_node =
    | Text : string -> _ react_node
    | Elem : tag Bindings.react_element Js.t -> _ react_node

  type 'a tree = 'a react_node list

  type 'a javascript_object = 'a Js.t

  type 'a component = (<  > Js.t as 'a)

  type

      'this

      class_spec =
    { render:
        this:'this component ->
        unit;  [@main]
        (* 'elem react_node; *)

            (* initial_state : *)
            (*   (this:'this component_api -> *)
            (*    'initial_state javascript_object) option; *)
            (* default_props : *)
            (*   (this:'this component_api -> *)
            (*    'default_props javascript_object) option; *)
            (* prop_types : 'prop_types javascript_object option; *)
            (* mixins : 'mixin javascript_object list option; *)
            (* statics : 'static_functions javascript_object option; *)
            display_name : string option;
            (* component_will_mount : (this:'this Js.t -> unit) option; *)
            (* component_did_mount : (this:'this Js.t -> unit) option; *)
            (* component_will_receive_props : *)
            (*   (this:'this Js.t -> next_prop:'next_props Js.t -> unit) option; *)
            (* should_component_update : *)
            (*   (this:'this Js.t -> *)
            (*    next_prop:'next_props Js.t -> *)
            (*    next_state:'next_state Js.t -> bool Js.t) option; *)
            (* component_will_update : *)
            (*   (this:'this Js.t -> *)
            (*    next_prop:'next_props Js.t -> *)
            (*    next_state:'next_state Js.t -> unit) option; *)
            (* component_did_update : *)
            (*   (this:'this Js.t -> *)
            (*    prev_prop:'prev_props Js.t -> *)
            (*    prev_state:'prev_state Js.t -> unit) option; *)
            (* component_will_unmount : (this:'this Js.t -> unit) option; *)
    } [@@deriving make]

end = Types

and Infix : sig

  (** Wrap an OCaml function for JavaScript to call. The first
      argument is the `this` context, the rest are whatever arguments
      the JavaScript side will call with. Example:

      !@(fun this -> print_endline *!this##.name)
  *)
  val ( !@ ) : ('a -> 'b) -> ('a, 'b) Js.meth_callback

  (** Inject any value as an any *)
  val ( !^ ) : 'a -> Js.Unsafe.any

  (** Get whatever is at the object, unsafe *)
  val ( <!> ) : 'any Types.javascript_object -> string -> 'b

  (** Get whatever is at the potentially null field *)
  val ( <?> ) : 'any Types.javascript_object -> string -> 'b option

  (** Get whatever is at the potentially undefined field *)
  val ( <@> ) : 'any Types.javascript_object -> string -> 'b option

  (** Merge two objects together *)
  val ( <+> ) :
    'any Types.javascript_object ->
    'any Types.javascript_object ->
    < .. > Js.t

  (** Call stringify on any JavaScript thing *)
  val ( !$ ) : 'a Js.t -> string

  (** Shorthand for {b Js.string} *)
  val ( !* ) : string -> Js.js_string Js.t

  (** Shorthand for {b Js.to_string} *)
  val ( !& ) : Js.js_string Js.t -> string

  (** Convert a JavaScript string array into an Ocaml list of strings *)
  val ( $> ) : Js.string_array Js.t -> string list

  (** Convert an OCaml list of strings into a JavaScript array of
      JavaScript strings *)
  val ( <$ ) : string list -> Js.js_string Js.t Js.js_array Js.t

  (** Mutate the object on the right hand side with whatever alist is
      on the left hand side, useful for making an object with key name
      conflicts with OCaml's reserved keywords, example:
      [(`class_name "container", !*"black")] >>> (object%js end)
  *)
  val ( >>> ) :
    (Types.attributes * Types.attr_t) list ->
    'b Types.javascript_object ->
    (((Types.attributes * Types.attr_t) list) * ('b Types.javascript_object))

  val ( >|> ) :
    (Types.attributes * Types.attr_t) list ->
    'b Types.javascript_object ->
    'b Types.javascript_object

end = struct
  let ( !@ ) = Js.wrap_meth_callback

  let ( !^ ) = Js.Unsafe.inject

  let ( <!> ) obj field = Js.Unsafe.get obj (Js.string field)

  let ( <?> ) obj field = Js.Opt.(obj <!> field |> return |> to_option)

  let ( <@> ) obj field = Js.Optdef.(obj <!> field |> return |> to_option)

  let ( <+> ) a b = Js.Unsafe.global##.Object##assign a b

  let ( !$ ) o = Js._JSON##stringify o |> Js.to_string

  let ( !* ) = Js.string

  let ( !& ) = Js.to_string

  let ( $> ) g =
    g |> Js.str_array |> Js.to_array
    |> Array.map ~f:Js.to_string |> Array.to_list

  let ( <$ ) g = g |> Array.of_list |> Array.map ~f:Js.string |> Js.array

  let ( >>> )
      (data : (Types.attributes * Types.attr_t) list) obj =
    data
    |> List.map ~f:(function
        | (`class_name s, o) -> (s, o)
        | (`id s, o)         -> (s, o))
    |> List.iter ~f:(fun (s, o) ->
        let with_key field  = Js.Unsafe.set obj !*s field in
        match o with
        | `s s_field -> with_key !*s_field
        | `i i_field -> with_key i_field
      );
    (data, obj)

  let ( >|> ) lhs rhs =
    lhs >>> rhs |> fun (_, obj) -> obj

end

and Bindings : sig

  class type react_dom_server = object

    constraint 'tag = Types.tag

    method renderToString :
      'tag react_element Js.t -> Js.js_string Js.t Js.meth
    method renderToStaticMarkup :
      'tag react_element Js.t -> Js.js_string Js.t Js.meth
  end

  and ['tag] react_element = object

    method type_ : Js.js_string Js.t Js.readonly_prop
    method key : 'a Js.t Js.Opt.t Js.prop
    (* method ref : react_element_ref Js.t Js.Opt.t Js.prop *)

  end

  and react_dom = object
    constraint 'tag = Types.tag

    method render :
      'tag react_element Js.t -> #Dom_html.element Js.t -> unit Js.meth

    method unmountComponentAtNode :
      #Dom_html.element Js.t -> bool Js.t Js.meth

    (* method findDOMNode : *)
    (*    Js.t -> #Dom_html.element Js.t Js.meth *)

  end

  and react_class = object

  end

  and ['this, 'tag] react = object

    constraint 'tag = Types.tag
    constraint 'this = _ Types.component

    method createElement_withString :
      Js.js_string Js.t -> 'tag react_element Js.t Js.meth

    (* method createElement_withPropsAndSingleText : *)
    (*   Js.js_string Js.t -> *)
    (*   <className: Js.js_string Js.t Js.readonly_prop> Js.t -> *)
    (*   Js.js_string Js.t -> *)
    (*   react_element Js.t Js.meth *)

    method createElement_WithReactClass :
      react_class Js.t -> _ Js.Opt.t -> 'tag react_element Js.t Js.meth

    method cloneElement : 'tag react_element Js.t -> 'tag react_element Js.t Js.meth
    method isValidElement : 'a Js.t -> bool Js.t Js.meth

    method createClass :
      <
        render :
          unit ->
          Types.tag Bindings.react_element Js.t Types.react_node Js.meth

          (* ('this, 'b Types.react_node) Js.meth_callback Js.readonly_prop *)

          (* ('this, 'tag react_element Js.t) Js.meth_callback Js.readonly_prop; *)

        (* getInitialState : *)
        (*   ('this, 'b Js.t) Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* getDefaultProps : *)
        (*   ('this, 'default_props Js.t ) *)
        (*     Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* propTypes : 'props_validator Js.t Js.Optdef.t Js.readonly_prop; *)
        (* mixins : 'mixin Js.t Js.js_array Js.t Js.Optdef.t Js.readonly_prop; *)
        (* statics : 'static_functions Js.t Js.Optdef.t Js.readonly_prop; *)
        (* displayName : Js.js_string Js.t Js.Optdef.t Js.readonly_prop; *)
        (* (\* Lifecycle Methods *\) *)
        (* componentWillMount : *)
        (*   ('this, unit) Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* componentDidMount : *)
        (*   ('this, unit) Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* componentWillReceiveProps : *)
        (*   ('this, *)
        (*    'next_props Js.t -> unit) *)
        (*     Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* shouldComponentUpdate : *)
        (*   ('this, 'next_props Js.t -> 'next_state Js.t -> bool Js.t) *)
        (*     Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* componentWillUpdate : *)
        (*   ('this, 'next_prop Js.t -> 'next_state Js.t -> unit) *)
        (*     Js.meth_callback  Js.Optdef.t Js.readonly_prop; *)
        (* componentDidUpdate : *)
        (*   ('this, *)
        (*    'prev_prop Js.t -> 'prev_state Js.t -> unit) *)
        (*     Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
        (* componentWillUnmount : *)
        (*   ('this, unit) *)
        (*     Js.meth_callback Js.Optdef.t Js.readonly_prop; *)
      > Js.t ->
      react_class Js.t Js.meth

    method createFactory :
      react_class Js.t ->
      (prop:'new_prop Js.t -> 'tag react_element Js.t) Js.meth

    method version : Js.js_string Js.t Js.readonly_prop
    (* method __spread *)
    method _DOM : 'a Js.t Js.readonly_prop
  end


end = Bindings

let string_of_tag tag =
  (Types.show_tag tag |> Js.string)##substring_toEnd 1 |> Js.to_string


(* module DOM = struct *)

(*   open Types *)
(*   open Infix *)

(*   let make *)
(*       ?(elem_attrs : (attributes * 'a javascript_object) list = []) *)
(*       ?(tag : Types.tag = `div) *)
(*       (children : 'elems Types.tree) *)
(*     = Infix.( *)
(*         let attrs = elem_attrs >|> object%js end in *)
(*         let elem_name = tag |> string_of_tag in *)
(*         let args = children |> List.map ~f:(function *)
(*             | Types.Text s -> !^(!*s) *)
(*             | Types.Elem e -> !^e *)
(*           ) |> Array.of_list *)
(*         in *)
(*         Js.Unsafe.meth_call *)
(*           Bindings.react##_DOM *)
(*           !*elem_name *)
(*           args *)

(*       ) *)

(* end *)

class react = object

  constraint 'tag = Types.tag

  constraint 'attributes = Types.attributes

  constraint 'elem = 'tag Bindings.react_element Js.t

  constraint 'attr_t = Types.attr_t

  constraint 'component = _ Types.component

  constraint 'other = _ Types.component

  constraint 'test = (<  > as 't) Js.t

  val __react : ('component, 'tag) Bindings.react Js.t =
    Js.Unsafe.([%require_or_default "react" global##.React])

  method create_class =
    fun (spec : 'test Types.class_spec) ->
      let open Js.Optdef in
      let open Types in
      let comp = (object%js(self)
        method render =
          fun this ->
            Js.debugger ();
            Text "123"
          (* spec.render ~this:(self :> < > Js.t) *)
          (* Js.wrap_meth_callback (fun this -> spec.render ~this) *)
      end)
      in
      __react##createClass (object%js
        method render = fun () ->
          print_endline "called render";
          Text "123"
      end)
    (* __react##createClass comp *)

  method make = fun
    ?(elem_attrs : (Types.attributes * Types.attr_t) list = [])
    ?(tag : Types.tag = `div)
    (children : 'elem Types.tree) :
    ('tag Bindings.react_element Js.t) Types.react_node ->
    Infix.(
      let attrs = elem_attrs >|> object%js end in
      let elem_name = tag |> string_of_tag in
      let args = (children |> List.map ~f:(function
          | Types.Text s -> !^(!*s)
          | Types.Elem e -> !^e
        ) |> fun l -> !^attrs :: l) |> Array.of_list
      in
      let e = Js.Unsafe.meth_call __react##._DOM elem_name args in
      Types.Elem e
    )

  method element_from_class class_ =
    __react##createElement_WithReactClass class_ Js.null

end

class react_dom = object

  (* constraint 'dom_elem = (#Dom_html.element as 'a) Js.t *)

  constraint 'dom_elem = Dom_html.element Js.t

  val __react_dom : Bindings.react_dom Js.t =
    Js.Unsafe.([%require_or_default "react-dom" global##.ReactDOM])

  method render ~react_elem (dom_elem : 'dom_elem) =
    __react_dom##render react_elem dom_elem

end

let (react, react_dom) = new react, new react_dom

module Helpers = struct
  let set_interval ~f ~every =
    Dom_html.window##setInterval (Js.wrap_callback f) every
  let get_elem ~id = Dom_html.getElementById id
  let debug thing field =
    Firebug.console##log
      (Js.Unsafe.(meth_call (get thing field) "toString" [||]))
  let real_type_name (o : 'a Js.t) =
    Infix.(!&((o <!> "constructor") <!> "name"))

end

(*   let (__react, __reactDOM, __reactDOMServer) : *)
(*     'react Js.t * 'react_dom Js.t * react_dom_server Js.t *)
(*     = Js.Unsafe.( *)
(*       [%require_or_default "react" global##.React], *)
(*       [%require_or_default "react-dom" global##.ReactDOM], *)
(*       [%require_or_default "react-dom/server" global##.dummy] *)
(*     ) *)

(*   let react : *)
(*     'this . *)
(*       (< isMounted : bool Js.t Js.meth; .. > as 'this) *)
(*       component_api react Js.t *)
(*     = __react *)

(*   let react_dom = __reactDOM *)

(*   (\* Only makes sense on the server, hence the unit *\) *)
(*   let react_dom_server : unit -> react_dom_server Js.t = *)
(*     fun () -> __reactDOMServer *)

(* end *)

(* type element_spec = { class_name: string option; } [@@deriving make] *)

(* let create_element : *)
(*   ?element_opts:element_spec -> *)
(*   string -> *)
(*   tree -> *)
(*   Low_level_bindings.react_element Js.t *)
(*   = fun ?element_opts elem_name children -> Js.Unsafe.( *)
(*       let g = *)
(*         children |> List.map ~f:(function *)
(*             | Elem e -> inject e *)
(*             | Text s -> Js.string s |> inject *)
(*             (\* | Fragment l ->  *\) *)
(*           ) *)
(*       in *)
(*       [ *)
(*         [| *)
(*           inject (Js.string elem_name); *)
(*           match element_opts with *)
(*             None -> Js.null |> inject *)
(*           | Some spec -> *)
(*             inject (object%js(self) *)
(*               val className = *)
(*               Js.Opt.(map (option spec.class_name) Js.string) *)
(*           end) *)
(*       |]; *)
(*       Array.of_list g *)
(*     ] *)
(*     |> Array.concat *)
(*     |> Js.Unsafe.meth_call Low_level_bindings.__react "createElement" *)
(*   ) *)

(* let create_element_from_class class_ = *)
(*   Low_level_bindings.react##createElement_WithReactClass class_ Js.null *)

(* let create_factory : *)
(*   Low_level_bindings.react_class Js.t -> *)
(*   (props:'a Js.t -> Low_level_bindings.react_element Js.t) = fun c -> *)
(*   let call_me = Low_level_bindings.react##createFactory c in *)
(*   fun ~props -> Js.Unsafe.fun_call call_me [|Js.Unsafe.inject props|] *)

let create_class class_opts =
  let open Js.Optdef in
  let open Types in
  let comp = (object%js
    (* Component Specifications *)
    val render =
      Js.wrap_meth_callback (fun this -> class_opts.render ~this)
    (* val getInitialState = *)
    (*   (fun f -> Js.wrap_meth_callback (fun this -> f ~this)) *)
    (*   |> map (option class_opts.initial_state) *)
    (* val getDefaultProps = *)
    (*   (fun f -> Js.wrap_meth_callback (fun this -> f ~this)) *)
    (*   |> map (option class_opts.default_props) *)
    (* val propTypes = option class_opts.prop_types *)
    (* val mixins = map (option class_opts.mixins) (fun m -> Array.of_list m |> Js.array) *)
    (* val statics = option class_opts.statics *)
    (* val displayName = map (option class_opts.display_name) Js.string *)
    (* Lifecycle Methods *)
    (* val componentWillMount = *)
    (*   (fun f -> Js.wrap_meth_callback (fun this -> f ~this)) *)
    (*   |> map (option class_opts.component_will_mount) *)
    (* val componentDidMount = *)
    (*   (fun f -> Js.wrap_meth_callback (fun this -> f ~this)) *)
    (*   |> map (option class_opts.component_did_mount) *)
    (* val componentWillReceiveProps = *)
    (*   (fun f ->  Js.wrap_meth_callback (fun this next_prop -> f ~this ~next_prop)) *)
    (*   |> map (option class_opts.component_will_receive_props) *)
    (* val shouldComponentUpdate = *)
    (*   (fun f -> Js.wrap_meth_callback *)
    (*       (fun this next_prop next_state -> f ~this ~next_prop ~next_state)) *)
    (*   |> map (option class_opts.should_component_update) *)
    (* val componentWillUpdate = *)
    (*   (fun f -> Js.wrap_meth_callback *)
    (*       (fun this next_prop next_state -> f ~this ~next_prop ~next_state)) *)
    (*   |> map (option class_opts.component_will_update) *)
    (* val componentDidUpdate = *)
    (*   (fun f ->  Js.wrap_meth_callback *)
    (*       (fun this prev_prop prev_state -> f ~this ~prev_prop ~prev_state)) *)
    (*   |> map (option class_opts.component_did_update) *)
    (* val componentWillUnmount = *)
    (*   (fun f -> Js.wrap_meth_callback (fun this -> f ~this)) *)
    (*   |> map (option class_opts.component_will_unmount) *)
  end)
  in
  ()
  (* react##createClass comp *)

(* let elem_from_spec spec = create_element_from_class (create_class spec) *)

(* let render ~react_elem dom_elem = *)
(*   Low_level_bindings.react_dom##render react_elem dom_elem *)


(* let (react, react_dom, react_dom_server) = *)
(*   Low_level_bindings.react, *)
(*   Low_level_bindings.react_dom, *)
(*   Low_level_bindings.react_dom_server *)

(* module DOM = struct *)

(*   type tag = [`a | `abbr | `address | `area | `article | `aside | `audio | *)
(*               `b | `base | `bdi | `bdo | `big | `blockquote | `body | *)
(*               `br | `button | `canvas | `caption | `cite | `code | *)
(*               `col | `colgroup | `data | `datalist | `dd | `del | *)
(*               `details | `dfn | `dialog | `div | `dl | `dt | `em | *)
(*               `emded | `fieldset | `figcaption | `figure | `footer | *)
(*               `form | `h1 | `h2 | `h3 | `h4 | `h5 | `h6 | `head | `header | *)
(*               `hgroup | `hr | `html | `i | `iframe | `img | `input | *)
(*               `ins | `kbd | `keygen | `label | `legend | `li | `link | *)
(*               `main | `map | `mark | `menu | `menuitem | `meta | `meter | *)
(*               `nav | `noscript | *)
(*               `object_ [@printer fun fmt -> fprintf fmt "`object"] | *)
(*               `ol | `optgroup | `option | `output | `p | `param | `picture | *)
(*               `pre | `progress | `q | `rp | `rt | `ruby | `s | `samp | *)
(*               `script | `section | `select | `small | `source | `span | *)
(*               `strong | `style | `sub | `summary | `sup | `table | *)
(*               `tbody | `td | `textarea | `tfoot | `th | `thead | *)
(*               `time | `title | `tr | `track | `u | `ul | `var | `video | *)
(*               `wbr | `circle | `clipPath | `defs | `ellipse | `g | *)
(*               `image | `line | `linearGradient | `mask | `path | *)
(*               `pattern | `polygon | `polyline | `radialGradient | *)
(*               `rect | `stop | `svg | `text | `tspan ] [@@deriving show] *)

(*   let string_of_tag tag = *)
(*     (show_tag tag |> Js.string)##substring_toEnd 1 |> Js.to_string *)

(*   type 'a elem_spec = *)
(*     (<className: Js.js_string Js.t Js.readonly_prop; .. > as 'a ) Js.t *)

(*   let make : *)
(*     ?elem_spec : 'a javascript_object -> *)
(*     ?class_name : string -> *)
(*     ?tag:tag -> *)
(*     tree -> *)
(*     Low_level_bindings.react_element Js.t *)
(*     = fun ?elem_spec ?class_name ?(tag=`div) children -> Js.Unsafe.(Infix.( *)
(*         let elem_name = tag |> string_of_tag in *)
(*         let args = children |> List.map ~f:(function *)
(*             | Elem e -> inject e *)
(*             | Text s -> Js.string s |> inject *)
(*             (\* | Fragment l -> *\) *)
(*           ) *)
(*         in *)
(*         match elem_spec, class_name with *)
(*         | None, None -> *)
(*           meth_call *)
(*             Low_level_bindings.react##._DOM *)
(*             elem_name *)
(*             (Array.of_list (inject Js.null :: args)) *)
(*         | (Some e_spec, None) -> *)
(*           meth_call *)
(*             Low_level_bindings.react##._DOM *)
(*             elem_name *)
(*             (Array.of_list ((inject e_spec) :: args)) *)
(*         | (Some e_spec, Some c_name) -> *)
(*           (\* Mutates the elem_spec by making className take *)
(*              precedence *\) *)
(*           meth_call *)
(*             Low_level_bindings.react##._DOM *)
(*             elem_name *)
(*             (Array.of_list ((inject ([("className", !*c_name)] >>> e_spec)) :: args)) *)
(*         | (None, Some c_name) -> *)
(*           meth_call *)
(*             Low_level_bindings.react##._DOM *)
(*             elem_name *)
(*             (Array.of_list ((inject (object%js val className = !*c_name end)) :: args)) *)
(*       )) *)

(* end *)

(* module Common_components = struct *)

(*   let stylesheet ?custom_opts ~href () = Infix.( *)
(*       let attrs = object%js *)
(*         val ref = !*"stylesheet" *)
(*         val href = !*href *)
(*       end *)
(*       in *)
(*       match custom_opts with *)
(*         None -> *)
(*         Elem (DOM.make ~elem_spec:attrs ~tag:`link []) *)
(*       | Some custom -> *)
(*         Elem (DOM.make ~elem_spec:(attrs <+> custom) ~tag:`link []) *)
(*     ) *)

(*   let ahref ?(href="") txt = Infix.( *)
(*       Elem (DOM.make *)
(*               ~elem_spec:(object%js val href = !*href end) *)
(*               ~tag:`a *)
(*               [Text txt]) *)
(*     ) *)

(* end *)
