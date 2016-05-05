module P = Printf

let (react, react_dom) =
  Js.Unsafe.global##.React,
  Js.Unsafe.global##.ReactDOM

let version = Runa.(react <!> "version" |> Js.to_string)

(* this is some insane typing *)
type element = {type_ : [`Dom_node_name of string |
                         `React_class of element];
                props : prop_t }
and prop_t = {fields : Js.Unsafe.any Jstable.t option;
              children : [`Nested_elem of element list |
                          `Plain_text of string list]}

let of_element elem = Runa.(
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

let button_example = Runa.(
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

let any_of_strs rest = Runa.(
   (rest |> List.map (fun s -> !!(Js.string s))) |> Array.of_list)

module React = struct

  type elem_arg =
    | React_class of Js.Unsafe.any
    | New_class of element

  let create_element = function
    | React_class c -> react##createElement c Js.null
    | New_class _ -> ()

  let create_class (module B : CREATE_REACT_CLASS) = Runa.(
      let open B in
      let with_fields = object%js
        val render = with_this (fun this ->
            match B.spec.render ~this:!!this with
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
            | Some f -> f ~this:!!this |> Js.Opt.return
          )
        val componentDidMount = with_this (fun this ->
            match B.spec.component_did_mount with
            | None -> Js.null
            | Some f -> f ~this:!!this |> Js.Opt.return
          )
        val componentWillReceiveProps = with_this (fun this ->
            match B.spec.component_will_receive_props with
            | None -> Js.null
            | Some f -> f ~this:!!this |> Js.Opt.return)
        val getInitialState = with_this (fun this ->
            match B.spec.get_initial_state with
            | None -> Js.null
            | Some f -> f ~this:!!this |> Js.Opt.return)
      end
      in
      react##createClass
        (merge with_fields
           (match B.spec.custom_fields with
              None -> (object%js end)
            (* Need to find a way to get with_this (fun this ->
               available to the functions in object_of_table's return
               value)*)
                        (* Js.typeof *)
            | Some t ->
              let obj = object_of_table t in
              obj

           ))
    )

  let create_factory app : Js.Unsafe.any -> Js.Unsafe.any =
    react##createFactory app

  let call_factory ?props factory = Runa.(
      [|match props with None -> !!Js.null | Some p -> !!(p |> Js.Opt.return)|]
      |> Js.Unsafe.fun_call factory
    )

  let simple_comp_spec ~html_node msg = R_CLASS.(
      {render = (fun ~this:_ -> {
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

  open Runa

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
    Jstable.add elem_obj (Js.string "onClick") !!(fun this -> this <!> "handleClick");
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
           get_initial_state = Some (fun ~this:_ ->
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

let _ = Examples_and_tutorials.ex_1 ()
