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
    ~component_will_receive_props:(fun ~this ~next_props ->
        print_endline "Component will receive props")
    (fun ~this ->
       let elapsed = Js.math##round this##.props##.elapsed /. 100.0 in
       let seconds = elapsed /. 10.0 in
       let message = Printf.sprintf
           "React has been successfully running for %f seconds" seconds
       in
       Reactjs.DOM.make ~tag:`p (`Text_nodes [message])
    )
  |> Reactjs.create_class

let _ =
  let example_app_factory = Reactjs.create_factory example_application in
  let start = (new%js Js.date_now)##getTime in
  Reactjs.set_interval
    ~f:(fun () ->
        try
          let with_new_props = example_app_factory ~props:(object%js
              val elapsed = (new%js Js.date_now)##getTime -. start
            end)
          in
          Reactjs.render with_new_props (Reactjs.get_elem ~id:"container")
        with Js.Error e ->
          Firebug.console##log e
      ) ~every:100.0
