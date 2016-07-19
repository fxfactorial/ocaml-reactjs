open Reactjs_high_level

let commentBox =
  create_class
    {render =
       (fun this_handle ->
          Firebug.console##log this_handle;
          create_element
            {element_name = "div";
             class_name = "commentBox";
             children = `Text_nodes ["Hello, world! I am a CommentBox"]}
       );
     display_name = "CommentBox";
     initial_state = None;
     default_props = None; }

let () =
  let id = Dom_html.getElementById in
  render (create_element_from_class commentBox) (id "content")
