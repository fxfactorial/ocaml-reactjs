open React_js

(* We first have to create a class and then we can use create_element
   to make instances of that class *)
let () =
  let commentbox =
    new component_spec
      ~display_name:"CommentBox"
      ~render_f:(fun () ->
          create_element
            (New_elem {html_elem_t = "div";
                       class_name = "commentBox";
                       content = "Hello world, I'm a commentbox"})
        )
      ()
    |> create_class
  in
  let comment_box_instance =
    create_element (React_class commentbox)
  in
  ignore (
    ReactDOM.render
      ~react_elem:comment_box_instance
      (Dom_html.getElementById "content")
  )
