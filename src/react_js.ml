module P = Printf

module Helpers = struct

  let ( <!> ) obj field = Js.Unsafe.get obj field
  let ( !@ ) f = Js.wrap_callback f
  let ( !! ) o = Js.Unsafe.inject o
  let stringify o = Js._JSON##stringify o |> Js.to_string
  let m = Js.Unsafe.meth_call

  let merge a b : < .. > Js.t = Js.Unsafe.global##.Object##assign a b

  let object_of_table t =
    let js_keys = Jstable.keys t in
    let values =
      js_keys
      |> List.map
        (fun k -> (Jstable.find t k |> Js.Optdef.get)
            (fun () -> assert false))
    in
    List.map2 (fun k v -> (Js.to_string k, v)) js_keys values
    |> Array.of_list
    |> Js.Unsafe.obj

  let debug item =
    (Js.Unsafe.global##.FOO := item) |> ignore

  let log l =
    Firebug.console##log l

  let set_interval ~every:float (callback: unit -> unit) =
    Dom_html.window##setInterval !@callback float

  let with_this f = !!(Js.wrap_meth_callback f)

end

let (react, react_dom) =
  Js.Unsafe.global##.React,
  Js.Unsafe.global##.ReactDOM

let version = Helpers.(react <!> "version" |> Js.to_string)

(* this is some insane typing *)
type element = {type_ : [`Dom_node_name of string |
                         `React_class of element];
                props : prop_t }
and prop_t = {fields : Js.Unsafe.any Jstable.t option;
              children : [`Nested_elem of element list |
                          `Plain_text of string]}

let of_element elem = Helpers.(
    let rec convert = function
      | {type_ = `Dom_node_name h;
         props = {fields = None;
                  children = `Plain_text s}} ->
        !!(object%js
          val type_ = !!(Js.string h)
          val props = (object%js
            val children = !!(Js.string s)
          end)
        end)
      | {type_ = `React_class e;
         props = {fields = None;
                  children = `Plain_text s}} ->
        !!(object%js
          val type_ = !!(convert e)
          val props = (object%js
            val children = !!(Js.string s)
          end)
        end)
      | _ -> !!(object%js end)
    in
    let t = convert elem in
    (* Lame *)
    Js.Unsafe.set t (Js.string "type") (Js.Unsafe.get t (Js.string "type_"));
    Js.Unsafe.delete t (Js.string "type_");
    t
  )

let button_example = Helpers.(
    let t = Jstable.create () in
    Jstable.add t (Js.string "className") !!"button button-blue";
    {type_ = `Dom_node_name "button";
     props = {fields = Some t;
              children = `Nested_elem [{type_ = `Dom_node_name "b";
                                        props = {fields = None;
                                                 children = `Plain_text "OK!";}
                                       }]}})

module type CREATE_REACT_CLASS = sig

  open Js.Unsafe

  type comp_spec =
    { render : this:any -> element;
      display_name : string option;
      component_will_mount : (this:any -> unit) option;
      component_did_mount : (this:any -> unit) option;
      component_will_receive_props :
        (this:any -> next_props:any -> unit) option;
      should_component_update :
        (this:any -> next_props:any -> next_state:any -> bool) option;
      component_will_update :
        (this:any -> next_props:any -> next_state:any -> unit) option;
      component_did_update :
        (this:any -> next_props:any -> next_state:any -> unit) option;
      component_will_unmount :
        (this:any -> next_props:any -> next_state:any -> unit) option;
      props : any Jstable.t option }

  val spec : comp_spec

end

module R_CLASS = struct
  open Js.Unsafe

  type comp_spec =
    { render : this:any -> element;
      display_name : string option;
      component_will_mount : (this:any -> unit) option;
      component_did_mount : (this:any -> unit) option;
      component_will_receive_props :
        (this:any -> next_props:any -> unit) option;
      should_component_update :
        (this:any -> next_props:any -> next_state:any -> bool) option;
      component_will_update :
        (this:any -> next_props:any -> next_state:any -> unit) option;
      component_did_update :
        (this:any -> next_props:any -> next_state:any -> unit) option;
      component_will_unmount :
        (this:any -> next_props:any -> next_state:any -> unit) option;
      props : any Jstable.t option }

end


module React = struct

  let create_class (module B : CREATE_REACT_CLASS) = Helpers.(
      let open B in
      let with_fields = object%js
        val render = with_this (fun this ->
            match B.spec.render ~this with
            | {type_ = `Dom_node_name node;
               props = {fields = None;
                        children = `Plain_text text}} ->
              react##createElement (Js.string node) Js.null (Js.string text)
            | _ -> B.spec.render ~this
          )
        val displayName = (match B.spec.display_name with
            | None -> Js.null
            | Some s -> Js.string s |> Js.Opt.return)
        val componentWillMount = with_this (fun this ->
            match B.spec.component_will_mount with
            | None -> Js.null
            | Some f -> f ~this |> Js.Opt.return
          )
        val componentDidMount = with_this (fun this ->
            match B.spec.component_did_mount with
            | None -> Js.null
            | Some f -> f ~this |> Js.Opt.return
          )
        val componentWillReceiveProps = with_this (fun this ->
            match B.spec.component_will_receive_props with
            | None -> Js.null
            | Some f -> f ~this |> Js.Opt.return)
      end
      in
      react##createClass
        (merge with_fields
           (match B.spec.props with None -> Js.null
                                  | Some t -> object_of_table t))
    )

  let create_factory app : Js.Unsafe.any -> Js.Unsafe.any =
    react##createFactory app

  let call_factory ?props factory = Helpers.(
      [|match props with None -> !!Js.null | Some p -> !!(p |> Js.Opt.return)|]
      |> Js.Unsafe.fun_call factory
    )

end


module Examples_and_tutorials = struct

  open Helpers

  let ex_1 = fun () ->
    let c = React.create_class (module struct include R_CLASS
        let spec =
          {render = (fun ~this ->
               let elapsed =
                 (((this <!> "props") <!> "elapsed") |> Js.to_float) /. 1000.0
               in
               {type_ = `Dom_node_name "p";
                props = {fields = None;
                         children = `Plain_text
                             (P.sprintf
                                "React has been successfully running for %f seconds"
                                elapsed)
                        }});
           display_name = None;
           component_will_mount = Some (fun ~this ->
               print_endline "About to mount"
             );
           component_did_mount = Some (fun ~this ->
               print_endline "Did Mount"
             );
           component_will_receive_props = Some (fun ~this ~next_props ->
               print_endline "Will receive props now"
             );
           should_component_update = Some (fun ~this ~next_props ~next_state ->
               print_endline "Should the component update";
               true);
           component_will_update = Some (fun ~this ~next_props ~next_state ->
               print_endline "Component will update"
             );
           component_did_update = Some (fun ~this ~next_props ~next_state ->
               print_endline "Component did update");
           component_will_unmount = Some (fun ~this ~next_props ~next_state ->
               print_endline "Will unmount");
           props = None}
      end)
    in
    let factory = React.create_factory c in
    let start = (new%js Js.date_now)##getTime in
    (fun () ->
       react_dom##render (React.call_factory ~props:(object%js
                            val elapsed = ((new%js Js.date_now)##getTime -. start)
                          end)
                            factory)
         (Dom_html.getElementById "content")
    )
    |> set_interval ~every:50.0

  (* let ex_2 = fun () -> *)
  (*   let counter = React.create_class (module struct include R_CLASS *)
  (*       let spec = *)
  (*         {render = (fun ~this -> *)
  (*              {type_ = `Dom_node_name "button"; *)
  (*               props = {fields = None; *)
  (*                       children = `Plain_text "Click me! Number of clicks: "}} *)
  (*            ); *)
  (*          display_name = None; *)
  (*          props = None} *)

  (*     end) *)
  (*   in *)
  (*   () *)


end

let _ = Examples_and_tutorials.ex_1 ()
