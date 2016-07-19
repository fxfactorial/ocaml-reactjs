ReactJS bindings in OCaml
============================

These are my bindings to ReactJS, by my count this is the global fifth
attempt at binding to React and my own second attempt, its kind of
hard.


Examples
=========


```ocaml
open Reactjs_high_level

let commentBox =
  with_default_options
    ~default_props:(fun _ ->
        object%js
          val some_words = Js.string "These are some things I wanted to pass Around"
        end
      )
    ~component_did_mount:(fun this ->
        Printf.sprintf "Pulling out of the props: %s"
          (this##.props##.some_words |> Js.to_string)
        |> print_endline
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
```

Compiles with:

```shell
$ ocamlfind ocamlc -package reactjs.high_level -linkpkg code.ml
$ js_of_ocaml a.out -o code.js
```
