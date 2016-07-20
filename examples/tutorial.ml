open Reactjs

let commentBox =
  with_default_options
    ~default_props:(fun _ ->
        object%js
          val some_words = Js.string "These are some things I wanted to pass Around"
        end
      )
    ~component_will_mount:(fun this ->

        if this##isMounted |> Js.to_bool
        then print_endline "Was mounted"
        else print_endline "Was not mounted";

        Printf.sprintf "Pulling out of the props: %s"
          (this##.props##.some_words |> Js.to_string)
        |> print_endline;
        try
          Printf.sprintf "Non existence from props: %s"
            (this##.props##.junk |> Js.to_string)
          |> print_endline
        with Js.Error e ->
          Printf.sprintf "Yay OCaml error handling: %s"
            (Js.to_string e##.message)
          |> print_endline

      )
    ~component_did_mount:(fun this ->
        if this##isMounted |> Js.to_bool
        then print_endline "Was mounted"
        else print_endline "Was not mounted";
      )
    ~render:(fun _ ->
        create_element
          {element_name = "div";
           class_name = "commentBox";
           children = `Text_nodes ["Hello, world! I am a CommentBox"]})
    "CommentBox"
  |> create_class

let () =
  let id = Dom_html.getElementById in
  render (create_element_from_class commentBox) (id "content")
