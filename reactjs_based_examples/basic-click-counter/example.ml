let counter = Reactjs.(
    make_class_spec
      ~initial_state:(fun ~this -> (object%js val count = 0 end))
      (fun ~this ->
         DOM.make
           (object%js
             val onClick = fun () ->
               this##setState (object%js val count = this##.state##.count + 1 end)
           end)
           ~tag:`button
           (`Text_nodes [Printf.sprintf
                           "Click me, number of clicks: %d"
                           this##.state##.count])
      )
    |> create_class
  )

let () =
  Reactjs.(render (create_element_from_class counter) (get_elem ~id:"container"))
