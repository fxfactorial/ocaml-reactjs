let counter = Reactjs.(
    make_class_spec
      ~initial_state:(fun ~this -> (object%js val count = 0 end))
      ~component_will_mount:(fun ~this ->
          print_endline "Component about to mount"
        )
      (fun ~this -> let open Reactjs.Infix in
        let handle_click = !@(fun () ->
            this##setState (object%js val count = this##.state##.count + 1 end))
        in
         DOM.make
           ~elem_spec:(object%js
             val onClick = handle_click
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
