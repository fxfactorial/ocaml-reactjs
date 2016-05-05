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

module type CREATE_CLASS = sig
  val render : < .. > Js.t -> element
  val props : Js.Unsafe.any Jstable.t
end


module React = struct

  let create_element e = react##createElement e Js.null

  let create_class (module B : CREATE_CLASS) = Helpers.(
      let with_render = object%js
        val render = !!((fun this ->
            match B.render this with
            | {type_ = `Dom_node_name node;
               props = {fields = None;
                        children = `Plain_text text}} ->
              react##createElement (Js.string node) Js.null (Js.string text)
            | _ -> B.render this
          )|> Js.wrap_meth_callback)
      end
      in
      react##createClass (merge with_render (object_of_table B.props))
    )


end

let () =
  let c = React.create_class (module struct
      (* Can use ppx extensions on this *)
      let render this =
        {type_ = `Dom_node_name "p";
         props = {fields = None;
                  children = `Plain_text "Hello World"}}
      let props = Jstable.create ()
    end)
  in

  react_dom##render (React.create_element c) (Dom_html.getElementById "content")
