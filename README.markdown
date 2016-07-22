ReactJS bindings in OCaml
============================

These are my bindings to ReactJS, by my count this is the global sixth
attempt at binding to React and my own second attempt, its kind of
hard.

Installation
==============

Right now you can install with:

```shell
$ opam pin add -y reactjs https://github.com/fxfactorial/ocaml-reactjs
```

The bindings should work on `node` or in the browser, both will assume
that `React`, and `ReactDOM` exist (on node they will do the
appropriate `require`, node side will also try to load the npm package
`react-dom-server`)

Examples
=========

These examples should be familiar...

```ocaml
let counter = Reactjs.(
    make_class_spec
      ~initial_state:(fun ~this -> (object%js val count = 0 end))
      ~component_will_mount:(fun ~this ->
          print_endline "Component about to mount"
        )
      (fun ~this ->
         DOM.make
           ~elem_spec:(object%js
             val onClick = fun () ->
               this##setState (object%js
                 val count = this##.state##.count + 1
               end)
           end)
           ~tag:`button
           (`Text [Printf.sprintf
                     "Click me, number of clicks: %d"
                     this##.state##.count])
      )
    |> create_class
  )

let () = Reactjs.(
    render
      ~react_elem:(create_element_from_class counter)
      (get_elem ~id:"container")
  )
```

...And here is how you can hook onto the lifecycle methods of a
ReactJS Component. Note features of the OCaml language that don't
exist in JavaScript, like labeled arguments and default values.

```ocaml
let example_application =
  Reactjs.make_class_spec
    ~initial_state:(fun ~this ->
        print_endline "Initial state called";
        object%js end
      )
    ~default_props:(fun ~this ->
        print_endline "Default props called";
        object%js end
      )
    ~component_will_mount:(fun ~this -> print_endline "Component will mount")
    ~component_did_mount:(fun ~this -> print_endline "Component did mount")
    ~component_will_receive_props:(fun ~this ~next_prop ->
        print_endline "Component will receive props"
      )
    ~should_component_update:(fun ~this ~next_prop ~next_state ->
        print_endline "Should component update called";
        Js.bool true
      )
    ~component_will_update:(fun ~this ~next_prop ~next_state ->
        print_endline "Component will update"
      )
    ~component_did_update:(fun ~this ~prev_prop ~prev_state ->
        print_endline "Component did update"
      )
    ~component_will_unmount:(fun ~this -> print_endline "Component about to unmount")
    (fun ~this ->
       let elapsed = Js.math##round this##.props##.elapsed /. 100.0 in
       let seconds = elapsed /. 10.0 in
       let message = Printf.sprintf
           "React has been successfully running for %f seconds" seconds
       in
       Reactjs.DOM.make ~tag:`p (`Text [message])
    )
  |> Reactjs.create_class

let _ =
  let example_app_factory = Reactjs.create_factory example_application in
  let start = (new%js Js.date_now)##getTime in
  Reactjs.set_interval
    ~f:(fun () ->
        try
          let react_elem = example_app_factory ~props:(object%js
              val elapsed = (new%js Js.date_now)##getTime -. start
            end)
          in
          Reactjs.render ~react_elem (Reactjs.get_elem ~id:"container")
        (* Get OCaml exception handling! *)
        with Js.Error e ->
          Firebug.console##log e
      ) ~every:100.0
```

Compiles any example with:

```shell
$ ocamlfind ocamlc -package reactjs -linkpkg code.ml
$ js_of_ocaml a.out -o code.js
```

...And see the other examples in directory `reactjs_based_examples`,
they are translations of the examples in the ReactJS source code.
