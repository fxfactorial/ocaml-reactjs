let debug thing field =
  Firebug.console##log (Js.Unsafe.meth_call (Js.Unsafe.get thing field) "toString" [||])

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
      'props,
      'this_record) component =
  { render: 'this Js.t -> Reactjs.react_element Js.t;
    initial_state : ('this Js.t -> 'initial_state Js.t) option;
    default_props : ('this Js.t -> 'default_props Js.t) option;
    prop_types : 'prop_types Js.t option;
    mixins :
      ('this Js.t -> ('this, 'initial_state, 'default_props, 'prop_types,
                      'static_functions, 'next_props, 'next_state,
                      'prev_props, 'prev_state, 'props, 'this_record) component list) option;
    statics : 'static_functions Js.t option;
    display_name : string;

    component_will_mount : ('this Js.t -> unit) option;

    component_did_mount : ('this Js.t -> unit) option;

    component_will_receive_props : ('this Js.t -> 'next_props Js.t -> unit) option;

    should_component_update :
      ('this Js.t -> 'next_props Js.t -> 'next_state Js.t -> bool Js.t) option;
    component_will_update :
      ('this Js.t -> 'next_props Js.t -> 'next_state Js.t -> unit) option;
    component_did_update :
      ('this Js.t -> 'prev_props Js.t -> 'prev_state Js.t) option;
    component_will_unmount : ('this Js.t -> unit) option;}


let with_default_options
    ?initial_state ?default_props ?prop_types
    ?mixins ?statics ?component_will_mount ?component_did_mount
    ?component_will_receive_props ?should_component_update
    ?component_will_update ?component_did_update
    ?component_will_unmount
    ~render display_name =
  {render; display_name; initial_state; default_props;prop_types; mixins;
   statics; component_will_mount; component_did_mount;
   component_will_receive_props; should_component_update;
   component_will_update; component_did_update; component_will_unmount;}

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
  let comp = (object%js
    val mutable render = Js.null
    val mutable getInitialState = Js.null
    val mutable getDefaultProps = Js.null
    (* val mutable propTypes = Js.null *)
    (* val mutable mixins = Js.null *)
    (* val mutable statics = Js.null *)
    val displayName = Js.string class_opts.display_name
    val mutable componentWillMount = Js.null
    val mutable componentDidMount = Js.null
  end)
  in
  (* Yay *)

  comp##.render :=
    Js.wrap_meth_callback (fun this -> class_opts.render this)
    |> return;

  comp##.componentWillMount :=
    Js.wrap_meth_callback
      (fun this -> map (option class_opts.component_will_mount) (fun f -> f this))
    |> return;

  comp##.componentDidMount :=
    Js.wrap_meth_callback
      (fun this -> map (option class_opts.component_did_mount) (fun f -> f this))
    |> return;

  comp##.getInitialState :=
    Js.wrap_meth_callback
      (fun this -> map (option class_opts.initial_state) (fun f -> f this))
    |> return;

  comp##.getDefaultProps :=
    Js.wrap_meth_callback
      (fun this -> map (option class_opts.default_props) (fun f -> f this))
    |> return;

  Reactjs.react##createClass comp
