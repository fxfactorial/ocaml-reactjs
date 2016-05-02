open React_js

let () =
  ignore (
    React_dom.render
      ~react_elem:(
        create_element
          ~children:(`Inner_html "This is the contents")
          ~class_name:"commentBox"
          (`Html_elem "div")
      )
      (Dom_html.getElementById "content")
  )

(* let box = object%js *)
(*   val hello = Js.string "Hello World" *)
(*   method func = fun item -> print_endline item *)
(* end *)

(* let () = *)
(*   box##func "Hello World" *)
