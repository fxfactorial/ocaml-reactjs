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

  let any_of_strs rest =
    (rest |> List.map (fun s -> !!(Js.string s))) |> Array.of_list

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
                          `Plain_text of string list]}

let of_element elem = Helpers.(
    let rec convert = function
      | {type_ = `Dom_node_name h;
         props = {fields = None;
                  (* Handle the arrayness,  *)
                  children = `Plain_text l}} ->
        !!(object%js
          val type_ = !!(Js.string h)
          val props = (object%js
            val children =
              if List.length l = 1 then !!(List.hd l |> Js.string)
              else !!(List.map Js.string l |> Array.of_list |> Js.array)
          end)
        end)
      | {type_ = `React_class e;
         props = {fields = None;
                  children = `Plain_text [s]}} ->
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
                                                 children = `Plain_text ["OK!"];}
                                       }]}})

module type CREATE_REACT_CLASS = sig

  open Js.Unsafe

  type comp_spec =
    { render : this:any -> element;
      display_name : string option;
      get_initial_state : (this:any -> any) option;
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
      custom_fields : any Jstable.t option }

  val spec : comp_spec

end

(* type 'b comp_spec = (module CREATE_REACT_CLASS with type comp_spec = 'b) *)

module R_CLASS = struct
  open Js.Unsafe

  type comp_spec =
    { render : this:any -> element;
      display_name : string option;
      get_initial_state : (this:any -> any) option;
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
      custom_fields : any Jstable.t option }

end


module React = struct

  type elem_arg =
    | React_class of Js.Unsafe.any
    | New_class of element

  let create_element = function
    | React_class c -> react##createElement c Js.null
    | New_class _ -> ()

  let create_class (module B : CREATE_REACT_CLASS) = Helpers.(
      let open B in
      let with_fields = object%js
        val render = with_this (fun this ->
            match B.spec.render ~this with
            | {type_ = `Dom_node_name node;
               props = {fields = None;
                        children = `Plain_text [text]}} ->
              react##createElement (Js.string node) Js.null (Js.string text)
            | {type_ = `Dom_node_name node;
               props = {fields = Some other_fields;
                        children = `Plain_text rest}} ->
              (Array.append
                 [|!!(Js.string node); (object_of_table other_fields)|]
                 (any_of_strs rest)
              )
              |> m react "createElement"
            (* Huge number of other cases missing *)
            (* | _ -> B.spec.render ~this *)
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
        val getInitialState = with_this (fun this ->
            match B.spec.get_initial_state with
            | None -> Js.null
            | Some f -> f ~this |> Js.Opt.return)
      end
      in
      react##createClass
        (merge with_fields
           (match B.spec.custom_fields with None -> Js.null
                                          | Some t -> object_of_table t))
    )

  let create_factory app : Js.Unsafe.any -> Js.Unsafe.any =
    react##createFactory app

  let call_factory ?props factory = Helpers.(
      [|match props with None -> !!Js.null | Some p -> !!(p |> Js.Opt.return)|]
      |> Js.Unsafe.fun_call factory
    )

  let simple_comp_spec ~html_node msg = R_CLASS.(
      {render = (fun ~this -> {
             type_ = `Dom_node_name html_node;
             props = {fields = None;
                      children = `Plain_text msg}});
       display_name = None;
       get_initial_state = None;
       component_will_mount = None;
       component_will_unmount = None;
       component_will_update = None;
       component_will_receive_props = None;
       component_did_update = None;
       component_did_mount = None;
       should_component_update = None;
       custom_fields = None;
      })


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
                             [P.sprintf
                                "React has been successfully running for %f seconds"
                                elapsed]
                        }});
           display_name = None;
           get_initial_state = None;
           component_will_mount = Some (fun ~this:_ ->
               print_endline "About to mount"
             );
           component_did_mount = Some (fun ~this:_ ->
               print_endline "Did Mount"
             );
           component_will_receive_props = Some (fun ~this:_ ~next_props:_ ->
               print_endline "Will receive props now"
             );
           should_component_update = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Should the component update";
               true);
           component_will_update = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Component will update"
             );
           component_did_update = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Component did update");
           component_will_unmount = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Will unmount");
           custom_fields = None}
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

  let ex_2 = fun () ->
    let other_fields = Jstable.create () in
    Jstable.add other_fields (Js.string "handleClick")
      (with_this (fun this ->
           Js.Unsafe.fun_call (this <!> "setState")
             [|!!(object%js
                 val count = ((this <!> "state") <!> "count") + 1
               end)|]
         ));
    let elem_obj = Jstable.create () in
    Jstable.add elem_obj (Js.string "onClick") (with_this (fun this ->
        this <!> "handleClick"
      ));
    let counter = React.create_class (module struct include R_CLASS
        let spec =
          {render = (fun ~this ->
               {type_ = `Dom_node_name "button";
                props = {fields = Some elem_obj;
                         children = `Plain_text ["Click me! Number of clicks: ";
                                                 ((this <!> "state") <!> "count")
                                                 |> string_of_int]
                        }}
             );
           display_name = None;
                      component_will_mount = Some (fun ~this:_ ->
               print_endline "About to mount"
             );
           get_initial_state = Some (fun ~this ->
               !!(object%js val count = 0 end)
             );
           component_did_mount = Some (fun ~this:_ ->
               print_endline "Did Mount"
             );
           component_will_receive_props = Some (fun ~this:_ ~next_props:_ ->
               print_endline "Will receive props now"
             );
           should_component_update = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Should the component update";
               true);
           component_will_update = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Component will update"
             );
           component_did_update = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Component did update");
           component_will_unmount = Some (fun ~this:_ ~next_props:_ ~next_state:_ ->
               print_endline "Will unmount");
           custom_fields = Some other_fields}

      end)
    in
    let e = React.create_element (React_class counter) in
    react_dom##render e (Dom_html.getElementById "content")

end

let _ = Examples_and_tutorials.ex_2 ()
