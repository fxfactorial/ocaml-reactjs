
let counter = Reactjs.(
    make_class_spec
      ~initial_state:(fun ~this -> (object%js val count = 0 end))
      (* ~default_props:(fun ~this -> *)
      (*     print_endline "Default props called"; *)
      (*     object%js end *)
      (*   ) *)
      (* ~component_will_mount:(fun ~this -> print_endline "Component will mount") *)
      (* ~component_did_mount:(fun ~this -> print_endline "Component did mount") *)
      (* ~component_will_receive_props:(fun ~this ~next_prop -> *)
      (*     print_endline "Component will receive props" *)
      (*   ) *)

      (* ~should_component_update:(fun ~this ~next_prop ~next_state -> *)
      (*     print_endline "Should component update called"; *)
      (*     Js.bool true *)
      (*   ) *)

      ~component_will_update:(fun ~this ~next_prop ~next_state ->
          print_endline "Component will update"
        )

      ~component_did_update:(fun ~this ~prev_prop ~prev_state ->
          print_endline "Component did update"
        )

      (* ~component_will_unmount:(fun ~this -> print_endline "Component about to unmount") *)

      (fun ~this ->

         DOM.make
           (object%js
             val onClick = (fun () ->
                 print_endline "In the call back";
                 this##setState (object%js val count = this##.state##.count + 1 end)
               )

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
