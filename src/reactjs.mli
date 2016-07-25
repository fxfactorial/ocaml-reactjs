(** OCaml bindings to ReactJS, interact with the function provided by
    the top level modules {b Infix}, {b Helpers}, and {b Reactjs} *)

type 'a javascript_object = 'a Js.t
(** A JavaScript object made via:
    (object%js
    val foo = Js.string "Hello"
    end) *)

(** Signature of a ReactComponent *)
type 'a component_api = 'a Js.t
  constraint 'a = < isMounted : bool Js.t Js.meth; .. >
    (** This constraint lets us guarantee what fields/methods will exist
        on the `this` object given to component lifecycle methods *)

(** Various helper functions that deal with the DOM API *)
module Helpers :
sig
  val set_interval :
    f:(unit -> unit) -> every:float -> Dom_html.interval_id

  (** alias of document.getElementById *)
  val get_elem : id:string -> Dom_html.element Js.t

  val debug : 'a -> 'b -> unit

  (** Calls constructor.name on any object *)
  val real_type_name : < .. > Js.t -> string
end

(* Nice trick to avoid having to type all those signatures again *)
include (module type of Helpers)

(** Infix and prefix functions for easier go between OCaml and
    JavaScript values *)
module Infix :
sig
  (** Wrap an OCaml function for JavaScript to call. The first
      argument is the `this` context, the rest are whatever arguments
      the JavaScript side will call with. Example:

      !@(fun this -> print_endline *!this##.name)
 *)
  val ( !@ ) : ('a -> 'b) -> ('a, 'b) Js.meth_callback

  (** Inject any value as an any *)
  val ( !^ ) : 'a -> Js.Unsafe.any

  (** Get whatever is at the object, unsafe *)
  val ( <!> ) : 'a Js.t -> string -> 'b

  (** Get whatever is at the potentially null field *)
  val ( <?> ) : 'a Js.t -> string -> 'b option

  (** Get whatever is at the potentially undefined field *)
  val ( <@> ) : 'a Js.t -> string -> 'b option

  (** Merge two objects together *)
  val ( <+> ) : 'a Js.t -> 'b Js.t -> < .. > Js.t

  (** Call stringify on any Object *)
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

      [("val", !*"black")] >>> (object%js end)
  *)
  val ( >>> ) : (string * 'a Js.t) list -> 'b Js.t -> 'b Js.t

end

(** These are the low level bindings, often times you don't need to
    interact with them. They follow the same API as presented from
    ReactJS itself at the JavaScript level *)
module Low_level_bindings : sig

  (** Only makes sense on nodejs, ReactDOMServer *)
  class type react_dom_server =
    object
      method renderToStaticMarkup :
        react_element Js.t -> Js.js_string Js.t Js.meth
      method renderToString :
        react_element Js.t -> Js.js_string Js.t Js.meth
    end

  (** ReactDOM *)
  and react_dom =
    object
      method render :
        react_element Js.t -> #Dom_html.element Js.t -> unit Js.meth
      method unmountComponentAtNode :
        #Dom_html.element Js.t -> bool Js.t Js.meth
    end

  (** React *)
  and ['c] react =
    object
      constraint 'c =
        (< isMounted : bool Js.t Js.meth; .. > as 'd) component_api
      method _DOM : 'a Js.t Js.readonly_prop
      method cloneElement :
        react_element Js.t -> react_element Js.t Js.meth
      method createClass :
        < componentDidMount : ('c, unit) Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          componentDidUpdate : ('c,
                                'prev_prop Js.t -> 'prev_state Js.t -> unit)
              Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          componentWillMount : ('c, unit) Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          componentWillReceiveProps : ('c, 'next_props Js.t -> unit)
              Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          componentWillUnmount : ('c, unit) Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          componentWillUpdate : ('c,
                                 'next_prop Js.t ->
                                 'next_state Js.t -> unit)
              Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          displayName : Js.js_string Js.t Js.Optdef.t Js.readonly_prop;
          getDefaultProps : ('c, 'default_props Js.t) Js.meth_callback
              Js.Optdef.t Js.readonly_prop;
          getInitialState : ('c, 'b Js.t) Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          mixins : 'mixin Js.t Js.js_array Js.t Js.Optdef.t
              Js.readonly_prop;
          propTypes : 'props_validator Js.t Js.Optdef.t Js.readonly_prop;
          render : ('c, react_element Js.t) Js.meth_callback
              Js.readonly_prop;
          shouldComponentUpdate : ('c,
                                   'next_props Js.t ->
                                   'next_state Js.t -> bool Js.t)
              Js.meth_callback Js.Optdef.t
              Js.readonly_prop;
          statics : 'static_functions Js.t Js.Optdef.t Js.readonly_prop >
          Js.t -> react_class Js.t Js.meth
      method createElement_WithReactClass :
        react_class Js.t -> 'e Js.Opt.t -> react_element Js.t Js.meth
      method createElement_withPropsAndSingleText :
        Js.js_string Js.t ->
        < className : Js.js_string Js.t Js.readonly_prop > Js.t ->
        Js.js_string Js.t -> react_element Js.t Js.meth
      method createElement_withString :
        Js.js_string Js.t -> react_element Js.t Js.meth
      method createFactory :
        react_class Js.t ->
        (prop:'new_prop Js.t -> react_element Js.t) Js.meth
      method isValidElement : 'a Js.t -> bool Js.t Js.meth
      method version : Js.js_string Js.t Js.readonly_prop
    end
  and react_element =
    object
      method key : 'a Js.t Js.Opt.t Js.prop
      method type_ : Js.js_string Js.t Js.readonly_prop
    end
  and react_class = object  end

  (** Handle on the React object *)
  val react :
    < isMounted : bool Js.t Js.meth; .. > component_api react Js.t

  (** Handle on ReactDOM *)
  val react_dom : 'a Js.t
  val react_dom_server : unit -> react_dom_server Js.t
end

type element_spec = { class_name : string option; }

val make_element_spec : ?class_name:string -> unit -> element_spec

(** A React node is one of these items *)
type react_node =
    Text of string
  (** A Plain text, this doesn't create a react element *)
  | Elem of Low_level_bindings.react_element Js.t
  (** An arbitrary ReactElement *)

(** Simple alias of a list of react_nodes *)
type tree = react_node list

(** Create and pass one of these records to {b create_class}, this is
    how you provide functions for a component lifecycle, props, state and
    other things. *)
type ('a, 'initial_state, 'default_props, 'prop_types, 'static_functions,
      'next_props, 'next_state, 'prev_props, 'prev_state, 'props, 'mixin)
    class_spec = {
  render : this:'a component_api -> Low_level_bindings.react_element Js.t;
  initial_state : (this:'a component_api -> 'initial_state Js.t) option;
  default_props : (this:'a component_api -> 'default_props Js.t) option;
  prop_types : 'prop_types Js.t option;
  mixins : 'mixin Js.t list option;
  statics : 'static_functions Js.t option;
  display_name : string option;
  component_will_mount : (this:'a Js.t -> unit) option;
  component_did_mount : (this:'a Js.t -> unit) option;
  component_will_receive_props :
    (this:'a Js.t -> next_prop:'next_props Js.t -> unit) option;
  should_component_update :
    (this:'a Js.t ->
     next_prop:'next_props Js.t -> next_state:'next_state Js.t -> bool Js.t)
      option;
  component_will_update :
    (this:'a Js.t ->
     next_prop:'next_props Js.t -> next_state:'next_state Js.t -> unit)
      option;
  component_did_update :
    (this:'a Js.t ->
     prev_prop:'prev_props Js.t -> prev_state:'prev_state Js.t -> unit)
      option;
  component_will_unmount : (this:'a Js.t -> unit) option;
} constraint 'a = < isMounted : bool Js.t Js.meth; .. >

(** Creates a class spec record for you to pass to
    {b create_class}. Only requires the last argument, which is your
    render function. To provide a function for a lifecycle method,
    simply provide a function to whatever optional function you
    want. *)
val make_class_spec :
  ?initial_state:(this:(< isMounted : bool Js.t Js.meth; .. > as 'a)
                      component_api ->
                  'b Js.t) ->
  ?default_props:(this:'a component_api -> 'c Js.t) ->
  ?prop_types:'d Js.t ->
  ?mixins:'e Js.t list ->
  ?statics:'f Js.t ->
  ?display_name:string ->
  ?component_will_mount:(this:'a Js.t -> unit) ->
  ?component_did_mount:(this:'a Js.t -> unit) ->
  ?component_will_receive_props:(this:'a Js.t -> next_prop:'g Js.t -> unit) ->
  ?should_component_update:(this:'a Js.t ->
                            next_prop:'g Js.t ->
                            next_state:'h Js.t -> bool Js.t) ->
  ?component_will_update:(this:'a Js.t ->
                          next_prop:'g Js.t -> next_state:'h Js.t -> unit) ->
  ?component_did_update:(this:'a Js.t ->
                         prev_prop:'i Js.t -> prev_state:'j Js.t -> unit) ->
  ?component_will_unmount:(this:'a Js.t -> unit) ->
  (this:'a component_api -> Low_level_bindings.react_element Js.t) ->
  ('a, 'b, 'c, 'd, 'f, 'g, 'h, 'i, 'j, 'k, 'e) class_spec

(** Create a ReactElement directly where you specify the element
    for the position argument *)
val create_element :
  ?element_opts:element_spec ->
  string -> tree -> Low_level_bindings.react_element Js.t

(** Create a ReactElement given a ReactClass *)
val create_element_from_class :
  Low_level_bindings.react_class Js.t ->
  Low_level_bindings.react_element Js.t

(** Create a React factory function *)
val create_factory :
  Low_level_bindings.react_class Js.t ->
  props:'a Js.t -> Low_level_bindings.react_element Js.t

(** Create a ReactClass, pass result of make_class_spec to this
    function *)
val create_class :
  (< isMounted : bool Js.t Js.meth; .. >, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i,
   'j)
    class_spec -> Low_level_bindings.react_class Js.t

(** Short cut for making a ReactElement given a {b class_spec} *)
val elem_from_spec :
  (< isMounted : bool Js.t Js.meth; .. >, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i,
   'j)
    class_spec -> Low_level_bindings.react_element Js.t

(** ReactDOM.render, pass the ReactElement and a Dom node to render in *)
val render :
  react_elem:Low_level_bindings.react_element Js.t ->
  #Dom_html.element Js.t -> 'a

(** Top level handle on React *)
val react :
  < isMounted : bool Js.t Js.meth; .. > component_api
    Low_level_bindings.react Js.t

(** Top level handle on ReactDOM *)
val react_dom : 'a Js.t

(** Top level handle on react server, a function call since this only
    makes sense on nodejs. *)
val react_dom_server : unit -> Low_level_bindings.react_dom_server Js.t

(** React.DOM *)
module DOM :
sig
  (** All the tags recognized by ReactJS *)
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
              `nav | `noscript | `object_ | `ol | `optgroup | `option | `output
             | `p | `param | `picture |`pre | `progress | `q | `rp | `rt
             | `ruby | `s | `samp | `script | `section | `select |
             `small | `source | `span |`strong | `style | `sub |
             `summary | `sup | `table | `tbody | `td | `textarea |
             `tfoot | `th | `thead | `time | `title | `tr | `track |
             `u | `ul | `var | `video | `wbr | `circle | `clipPath |
             `defs | `ellipse | `g | `image | `line | `linearGradient
             | `mask | `path | `pattern | `polygon | `polyline |
             `radialGradient | `rect | `stop | `svg | `text | `tspan ]

  val string_of_tag : tag -> string
  type 'a elem_spec = 'a Js.t
    constraint 'a = < className : Js.js_string Js.t Js.readonly_prop; .. >

  (** Create your ReactElement *)
  val make :
    ?elem_spec:'a javascript_object ->
    tag:tag -> tree -> Low_level_bindings.react_element Js.t
end

(** Helper functions to create commonly needed components *)
module Common_components : sig

  (** Creates a `link element, offers you a chance to add other
      attributes to the stylesheet. *)
  val stylesheet : ?custom_opts:'a Js.t -> href:string -> unit -> react_node

end
