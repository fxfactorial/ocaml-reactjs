open React_js

let comment_box =
  let com_spec  =
    new component_spec
      ~render_f:(fun () -> new react_element)
      ~class_name:"commentBox"
  in
  create_class com_spec



(* let box = object%js *)
(*   val hello = Js.string "Hello World" *)
(*   method func = fun item -> print_endline item *)
(* end *)

(* let () = *)
(*   box##func "Hello World" *)