module P = Printf

let (react, react_dom) =
  Js.Unsafe.global##.React,
  Js.Unsafe.global##.ReactDOM

let version = Runa.(react <!> "version" |> Js.to_string)

type this

module React = struct

  class react_class =
    let () =
      let obj = object%js
        val hack = Js.wrap_meth_callback (fun this ->
            let open Runa in
            (match this <*> "other_value" with
               None -> print_endline "couldn't pull value"
             | Some s -> print_endline (Js.to_string s));
            this
          )
      end
      in
      let obj2 = object%js
        val other_value = Js.string "Hello world"
      end
      in
      let obj_merged = Runa.merge obj obj2 in
      Runa.debug obj_merged
    in
    object
      initializer
        print_endline "Created element"
    end


  module DOM = struct


  end

end


module ReactDOM = struct

  let render
      ?on_update_or_render:(cb : (this -> unit) option)
      react_elem
      (dom_elem : #Dom_html.element Js.t) =
    let cb_func = match cb with
        None -> Js.null
      | Some f ->
        Js.wrap_meth_callback (fun this -> f this)
        |> Js.Opt.return
    in
    react_dom##render react_elem dom_elem cb_func

end

(* let _ = let open Runa in *)
  (* let props = object%js val className = Js.string "commentBox" end in *)
  (* let children = Js.string "Hello world" in *)
  (* let elem = React.DOM.create ~props ~children `p in *)
  (* ReactDOM.render *)
  (*   ~on_update_or_render:(fun this -> *)
  (*       match this <*> "className" with *)
  (*       | None -> print_endline "No class name" *)
  (*       | Some name -> log name *)
  (*     ) *)
  (*   elem *)
  (*   (Dom_html.getElementById "content") *)

let _ = let open Runa in
  let e = new React.react_class in
  ()
