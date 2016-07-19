type element_opts =
  { element_name : string;
    class_name: string;
    children: [`Text_nodes of string list
              | `React_children of Reactjs.react_element Js.t list ]; }

type ('this,
      'initial_state,
      'default_props,
      'prop_types,
      'static_functions,
      'next_props,
      'next_state,
      'prev_props,
      'prev_state,
      'misc_object) component =
  { render: 'this Js.t -> Reactjs.react_element Js.t;
    initial_state : ('this Js.t -> 'initial_state Js.t) option;
    default_props : ('this Js.t -> 'default_props Js.t) option;
    prop_types : ('this Js.t -> 'prop_types Js.t) option;
    mixins :
      ('this Js.t -> ('this, 'initial_state, 'default_props, 'prop_types,
                      'static_functions, 'next_props, 'next_state,
                      'prev_props, 'prev_state, 'misc_object) component list) option;
    statics : ('this Js.t -> 'static_functions Js.t) option;
    display_name : ('this Js.t -> string);
    component_will_mount : ('this Js.t -> unit) option;
    component_did_mount : ('this Js.t -> unit) option;
    component_will_receive_props : ('this Js.t -> 'next_props Js.t -> unit) option;
    should_component_update :
      ('this Js.t -> 'next_props Js.t -> 'next_state Js.t -> bool Js.t) option;
    component_will_update :
      ('this Js.t -> 'next_props Js.t -> 'next_state Js.t -> unit) option;
    component_did_update :
      ('this Js.t -> 'prev_props Js.t -> 'prev_state Js.t) option;
    component_will_unmount : ('this Js.t -> unit) option;
    misc : ('this Js.t -> 'misc_object Js.t) option;}

let with_default_options
    ?initial_state ?default_props ?prop_types
    ?mixins ?statics ?component_will_mount ?component_did_mount
    ?component_will_receive_props ?should_component_update
    ?component_will_update ?component_did_update
    ?component_will_unmount ?misc
    ~render ~display_name () =
  {render; display_name; initial_state; default_props;prop_types; mixins;
   statics; component_will_mount; component_did_mount;
   component_will_receive_props; should_component_update;
   component_will_update; component_did_update;
   component_will_unmount; misc}

let create_element element_opts =
  let arr =
    (match element_opts.children with
     | `Text_nodes s -> List.map Js.string s
     | _ -> [])
    |> Array.of_list |> Array.map Js.Unsafe.inject
  in
  (Array.append
     [|
       Js.Unsafe.inject ((Js.string element_opts.element_name));
       Js.Unsafe.inject (object%js(self)
         val className = Js.string element_opts.class_name
       end);
     |]
     arr
  )
  |> Js.Unsafe.meth_call Reactjs.__react "createElement"

let create_element_from_class class_ =
  Reactjs.react##createElement_WithReactClass class_ Js.null

let render element dom_elem =
  Reactjs.reactDOM##render element dom_elem

let create_class class_opts = let open Js.Opt in
  let comp = (object%js (self)
    method render = class_opts.render self
    method getInitialState =
      map (option class_opts.initial_state) (fun f -> f self)
    method getDefaultProps =
      map (option class_opts.default_props) (fun f -> f self)
    val mutable propTypes = Js.null
    val mutable mixins = Js.null
    val mutable statics = Js.null
    val mutable displayName = Js.string "NOT_SET_YET"
    val mutable componentWillMount = Js.null
    val mutable componentDidMount = Js.null
  end)
  in
  (* Yay *)
  comp##.displayName := class_opts.display_name comp |> Js.string;

  comp##.componentWillMount :=
    Js.Opt.return (
      Js.wrap_meth_callback
        (fun _ -> map (option class_opts.component_will_mount) (fun f -> f comp)));

  comp##.componentDidMount :=
    Js.Opt.return (
      Js.wrap_meth_callback
        (fun _ -> map (option class_opts.component_did_mount) (fun f -> f comp)));

  Reactjs.react##createClass comp
