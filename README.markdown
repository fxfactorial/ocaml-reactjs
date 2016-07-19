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
  Reactjs_high_level.with_default_options
    ~component_will_mount:(fun this -> print_endline "Component About to mount")
    ~component_did_mount:(fun this ->
        Printf.sprintf "My name is: %s" (this##.displayName |> Js.to_string)
        |> print_endline)
    ~render:(fun this -> create_element
                {element_name = "div";
                 class_name = "commentBox";
                 children = `Text_nodes ["Hello, world! I am a CommentBox"]})
    ~display_name:(fun this -> "CommentBox") ()
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
