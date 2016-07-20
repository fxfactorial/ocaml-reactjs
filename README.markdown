ReactJS bindings in OCaml
============================

These are my bindings to ReactJS, by my count this is the global fifth
attempt at binding to React and my own second attempt, its kind of
hard.


Examples
=========


```ocaml
open Reactjs

let commentBox =
  make_class_spec
    ~default_props:(fun _ ->
        object%js
          val some_words = Js.string "These are some things I wanted to pass Around"
          method call_me = print_endline "Function inside the prop called"
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

        this##.props##call_me;

      )
    ~display_name:"CommentBox"
    (fun _ ->
       DOM.p (`Text_nodes
                ["Hello, world!";
                 "I am a Comment box in a p tag"]))
  |> create_class

let () =
  let id = Dom_html.getElementById in
  render (create_element_from_class commentBox) (id "content")
```

Compiles with:

```shell
$ ocamlfind ocamlc -package reactjs -linkpkg code.ml
$ js_of_ocaml a.out -o code.js
```
