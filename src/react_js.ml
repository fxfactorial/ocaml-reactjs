module P = Printf

let (react, react_dom) =
  Js.Unsafe.global##.React,
  Js.Unsafe.global##.ReactDOM

type this

let version = Runa.(react <!> "version" |> Js.to_string)

module React = struct

  type 'a react_element = { raw_js : 'a; }

  type 'a react_class = { raw_js : 'a; }

  let create_class
      ?props:(props_func : (this -> 'a) option)
      ~render:(render : (this -> 'a react_element)) : 'b react_class =
    let wrap = Js.wrap_meth_callback (fun this -> render this) in
    let with_render = object%js val render = wrap end in
    let this = Js.Unsafe.eval_string "this" in
    {raw_js = match props_func with
        | None ->
          this |> Runa.log;
          react##createClass with_render
        | Some p ->
          react##createClass (Runa.merge with_render (p this))
    }

  module DOM = struct

    type elem_t = [`p | `div | `ul | `li | `h2 | `h4 | `h5]

    let create ?props ?children (elem : elem_t) : 'a react_element =
      let (props_, kids) = Js.Opt.(option props, option children) in
      let this = Js.Unsafe.js_expr "this" in
      Runa.log this;
      {raw_js = match elem with
          | `p -> react##.DOM##p props_ kids
          | `div -> react##.DOM##div props_ kids
          | `ul -> react##.DOM##p props_ kids
          | `li -> react##.DOM##p props_ kids
          | `h2 -> react##.DOM##p props_ kids
          | `h4 -> react##.DOM##p props_ kids
          | `h5 -> react##.DOM##p props_ kids
      }

  end

end


module ReactDOM = struct

  let render
      ?on_update_or_render:(cb : (this -> unit) option)
      (react_elem : 'a React.react_element)
      (dom_elem : #Dom_html.element Js.t) =
    let cb_func = match cb with
        None -> Js.null
      | Some f ->
        Js.wrap_meth_callback (fun this -> f this)
        |> Js.Opt.return
    in
    react_dom##render react_elem.React.raw_js dom_elem cb_func

  let find_dom_node () = ()

end

let _ = let open Runa in
  let props = object%js val className = Js.string "commentBox" end in
  let children = Js.string "Hello world" in
  let elem = React.DOM.create ~props ~children `p in
  ReactDOM.render
    ~on_update_or_render:(fun this ->
        match this <*> "className" with
        | None -> print_endline "No class name"
        | Some name -> log name
      )
    elem
    (Dom_html.getElementById "content")

(* let _ = let open Runa in *)
(*   let counter = React.create_class q *)
(*   () *)
