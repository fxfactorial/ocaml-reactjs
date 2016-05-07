module P = Printf

let (react, react_dom) =
  Js.Unsafe.global##.React,
  Js.Unsafe.global##.ReactDOM

let version = Runa.(react <!> "version" |> Js.to_string)

module React = struct

  (* A whole bunc more to go *)
  type component_spec = { initial_state : (Runa.this -> Js.Unsafe.any) option;
                          default_props : (Runa.this -> Js.Unsafe.any) option;}

  let simple_defaults =
    { initial_state = None;
      default_props = None; }

  type new_elem = { elem_name : string;
                    children : string; }

  let dummy_elem = {elem_name = ""; children = ""; }

  class react_element (arg : [`React_class of react_class
                             | `New_elem of new_elem ]) =
    let _elem_ = match arg with
      | `React_class e ->
        react##createElement e#unsafe_raw Js.null
      | `New_elem {elem_name; children} ->
        react##createElement
          (Js.string elem_name)
          (object%js end)
          (Js.string children)
    in
    object
      val mutable raw_js = _elem_
      method unsafe_raw = Runa.(!!raw_js)
      method unsafe_set_raw (new_raw : Js.Unsafe.any) = raw_js <- new_raw

    end

  and react_class
      ?com_spec:(com_spec = (simple_defaults :  component_spec))
      ~render:(render_f : Runa.this -> react_element)
      ~display_name:d
      extras
    =
    let _class_ =
      let open Runa in
      let obj_render = object%js
        val displayName = Js.string d
        val render =
          (fun this -> (render_f this)#unsafe_raw)
          |> Js.wrap_meth_callback
        val getInitialState = Runa.when_f com_spec.initial_state
        val getDefaultProps = Runa.when_f com_spec.default_props
      end
      in
      let obj_merged =
        (match extras with None -> object%js end | Some p -> object_of_table p)
        |> merge obj_render
      in
      let class_ = react##createClass obj_merged in
      class_
    in
    object
      method unsafe_raw = Runa.(!!_class_)
    end

  let create_class
      ?com_spec:(com_spec = (simple_defaults :  component_spec))
      ~render:(render : Runa.this -> react_element)
      ~display_name:display_name
      extras =
    new react_class ~com_spec ~render ~display_name extras

  let create_element (arg : [`React_class of react_class
                            | `New_elem of new_elem ]) =
    new react_element arg

  let create_factory (arg : [`React_class of react_class
                            | `Dom_node_name of string]) :
    Js.Unsafe.any -> react_element = match arg with
    | `React_class c -> fun options ->
      let fact = react##createFactory c#unsafe_raw in
      let dummy = new react_element (`New_elem dummy_elem) in
      let result = Js.Unsafe.fun_call fact [|options|] in
      dummy#unsafe_set_raw result;
      dummy
    | `Dom_node_name node -> fun options ->
      react##createFactory (Js.string node)


  module DOM = struct


  end

end


module ReactDOM = struct

  let render
      ?on_update_or_render:(cb : (Runa.this -> unit) option)
      (react_elem : React.react_element)
      (dom_elem : #Dom_html.element Js.t) =
    let cb_func = match cb with
        None -> Js.null
      | Some f ->
        Runa.with_this f
        |> Js.Opt.return
    in
    react_dom##render react_elem#unsafe_raw dom_elem cb_func

end

(* let _ = let open React in *)
(*   let open Runa in *)
(*   let comment_box = *)
(*     create_class *)
(*       ~com_spec:{initial_state = Some (fun _ -> *)
(*           !!(object%js val count = 0 end) *)
(*         ); *)
(*          default_props = None;} *)
(*       ~display_name:"Comment Box" *)
(*       ~render:(fun this -> *)
(*           `New_elem {elem_name = "p"; *)
(*                      children = "Hello World"} *)
(*           |> create_element *)
(*         ) *)
(*       None *)
(*   in *)
(*   ReactDOM.render *)
(*     (create_element (`React_class comment_box)) *)
(*     (Dom_html.getElementById "content") *)

let _ =
  let open React in
  let open Runa in
  let example_application = create_class
      ~render:(fun this ->
          let seconds =
            Js.math##round (((this <!> "props") <!> "elapsed") /. 100.0)
          in
          let msg =
            P.sprintf "React has been running for %f seconds" seconds
          in
          `New_elem {elem_name = "p"; children = msg}
          |> create_element
        )
      ~display_name:"App"
      None
  in
  let app_factory = create_factory (`React_class example_application) in
  let start = time_now () in
  (fun () ->
     ReactDOM.render
       (app_factory !!(object%js
          val elapsed = time_now () -. start
        end))
       (Dom_html.getElementById "content")
  )
  |> set_interval ~every:50.0
