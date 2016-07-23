open StdLabels

let quadratic_calculator = Reactjs.(
    make_class_spec
      ~initial_state:(fun ~this -> object%js
                       val a = 1.0 val b = 3.0 val c = -3.0
                     end)
      (fun ~this -> let open Infix in
        let handle_input_change =
          fun ~key event ->
            let new_state =
              ([(key,
                 event##.target##.value |> Js.parseFloat |> Js.number_of_float )] >>>
               object%js end)
            in
            this##setState new_state
        in
        let (a, b, c) = this##.state##.a, this##.state##.b, this##.state##.c in
        let root = Js.math##sqrt ((Js.math##pow b 2.0) -. 4.0 *. a *. c) in
        let denominator = 2.0 *. a in
        let (x1, x2) = (-.b +. root) /. denominator, (-.b -. root) /. denominator in
        let input_label ~key init_value = DOM.(
            make ~tag:`label
              [Text (Printf.sprintf "%s: " key);
               Elem (make ~elem_spec:(object%js
                       val type_ = Js.string "number"
                       val value = !^init_value
                       val onChange = handle_input_change ~key
                     end) ~tag:`input [])]
          )
        in
        let label_row l = l |> List.map ~f:(fun (key, value) ->
            [Elem (input_label ~key value); Elem (DOM.make ~tag:`br [])]
          ) |> List.flatten
        in
        let equation_row = DOM.(
            [Elem (make ~tag:`em [Text "ax"]); Elem (make ~tag:`sup [Text "2"]);
             Text " + "; Elem (make ~tag:`em [Text "bx"]); Text " + ";
             Elem (make ~tag:`em [Text "c"]); Text " = 0"])
        in
        DOM.(make ~tag:`div
               [Elem (make ~tag:`strong equation_row );
                Elem (make ~tag:`h4 [Text "Solve for ";
                                     Elem (make ~tag:`em [Text "x"])]);
                Elem (make ~tag:`p
                        (label_row [("a", a); ("b", b); ("c", c)] @
                         [Text "x: ";
                          Elem (make ~tag:`strong
                                  [Text (Printf.sprintf "%f %f" x1 x2)])]))
               ]))
    |> create_class
  )

let () =
  Reactjs.(render
             ~react_elem:(create_element_from_class quadratic_calculator)
             (get_elem ~id:"container"))
