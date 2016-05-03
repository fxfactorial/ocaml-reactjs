open React_js

(* We first have to create a class and then we can use create_element
   to make instances of that class *)
let () =
  let commentbox =
    new component_spec
      ~display_name:"CommentBox"
      ~render_f:(fun () ->
          create_element
            New_elem {html_elem_t = "div";
                      class_name = "commentBox";
                      content = "Hello world, I'm a commentbox"}
            (* ~children:(`Inner_content "Hello world, I'm a commentbox") *)
            (* ~class_name:"commentBox" *)
            (* (`Html_elem "div") *)
        )
      ()
    |> create_class
  in
  ()
  (* let comment_box_instance = *)
  (*   create_element ~children:(`Inner_conent) *)

  (* ignore ( *)
  (*   ReactDOM.render *)
  (*     ~react_elem:( *)
  (*       create_element *)

  (*     ) *)
  (*     (Dom_html.getElementById "content") *)
  (* ) *)

  (* ignore ( *)
  (*   ReactDOM.render *)
  (*     ~react_elem:( *)
  (*       create_element *)
  (*         ~children:(`Inner_content "This is the contents") *)
  (*         ~class_name:"commentBox" *)
  (*         (`Html_elem "div") *)
  (*     ) *)
  (*     (Dom_html.getElementById "content") *)
  (* ) *)

(* let box = object%js *)
(*   val hello = Js.string "Hello World" *)
(*   method func = fun item -> print_endline item *)
(* end *)

(* let () = *)
(*   box##func "Hello World" *)
