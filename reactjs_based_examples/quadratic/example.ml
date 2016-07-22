(* var QuadraticCalculator = React.createClass({ *)
(*   getInitialState: function() { *)
(*     return { *)
(*       a: 1, *)
(*       b: 3, *)
(*       c: -4 *)
(*     }; *)
(*   }, *)

(*   /** *)
(*    * This function will be re-bound in render multiple times. Each .bind() will *)
(*    * create a new function that calls this with the appropriate key as well as *)
(*    * the event. The key is the key in the state object that the value should be *)
(*    * mapped from. *)
(*    */ *)
(*   handleInputChange: function(key, event) { *)
(*     var partialState = {}; *)
(*     partialState[key] = parseFloat(event.target.value); *)
(*     this.setState(partialState); *)
(*   }, *)

(*   render: function() { *)
(*     var a = this.state.a; *)
(*     var b = this.state.b; *)
(*     var c = this.state.c; *)
(*     var root = Math.sqrt(Math.pow(b, 2) - 4 * a * c); *)
(*     var denominator = 2 * a; *)
(*     var x1 = (-b + root) / denominator; *)
(*     var x2 = (-b - root) / denominator; *)
(*     return ( *)
(*       <div> *)
(*       <strong> *)
(*       <em>ax</em><sup>2</sup> + <em>bx</em> + <em>c</em> = 0 *)
(*       </strong> *)
(*       <h4>Solve for <em>x</em>:</h4> *)
(*       <p> *)
(*       <label> *)
(*       a: <input type="number" value={a} onChange={this.handleInputChange.bind(null, 'a')} /> *)
(*       </label> *)
(*       <br /> *)
(*       <label> *)
(*       b: <input type="number" value={b} onChange={this.handleInputChange.bind(null, 'b')} /> *)
(*       </label> *)
(*       <br /> *)
(*       <label> *)
(*       c: <input type="number" value={c} onChange={this.handleInputChange.bind(null, 'c')} /> *)
(*       </label> *)
(*       <br /> *)
(*       x: <strong>{x1}, {x2}</strong> *)
(*       </p> *)
(*       </div> *)
(*     ); *)
(*   } *)
(* }); *)

let quadratic_calculator = Reactjs.(
    make_class_spec
      ~initial_state:(fun ~this ->
          object%js
            val a = 1.0 val b = 3.0 val c = -3.0
          end
        )
      (fun ~this -> let open Infix in
        let handle_input_change =
          fun ?(key="") event ->
            Firebug.console##log event;
            Js.Unsafe.(obj [|(key, !^(Js.parseFloat event##.target##.value))|])
            |> this##setState
        in

        let (a, b, c) = this##.state##.a, this##.state##.b, this##.state##.c in
        let root = Js.math##sqrt ((Js.math##pow b 2.0) -. 4.0 *. a *. c) in
        let (denominator, x1, x2) = 2.0 *. a, -.b +. root, -.b -. root in
        DOM.(make ~tag:`div
               [Elem (make ~tag:`strong
                        [Elem (make ~tag:`em [Text "ax"]);
                         Elem (make ~tag:`sup [Text "2"]);
                         Text " + ";
                         Elem (make ~tag:`em [Text "bx"]);
                         Text " + ";
                         Elem (make ~tag:`em [Text "c"]);
                         Text " = 0"]);
                Elem (make ~tag:`h4 [Text "Solve for "; Elem (make ~tag:`em [Text "x"])]);
                Elem (make ~tag:`p
                        [Elem (make ~tag:`label
                                 [Text "a: ";
                                  Elem (make
                                          ~elem_spec:(object%js
                                            val type_ = !*(string_of_float a)
                                            val onChange = !@(handle_input_change ~key:"A")
                                          end)
                                          ~tag:`input [])])])
               ]))
    |> create_class
  )

let () =
  Reactjs.(render
             ~react_elem:(create_element_from_class quadratic_calculator)
             (get_elem ~id:"container"))
