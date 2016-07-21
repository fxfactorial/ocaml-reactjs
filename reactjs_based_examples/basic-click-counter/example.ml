     (* var Counter = React.createClass({ *)
     (*   getInitialState: function () { *)
     (*     return { count: 0 }; *)
     (*   }, *)
     (*   handleClick: function () { *)
     (*     this.setState({ *)
     (*       count: this.state.count + 1, *)
     (*     }); *)
     (*   }, *)
     (*   render: function () { *)
     (*     return ( *)
     (*       <button onClick={this.handleClick}> *)
     (*         Click me! Number of clicks: {this.state.count} *)
     (*       </button> *)
     (*     ); *)
     (*   } *)
     (* }); *)
     (* ReactDOM.render( *)
     (*   <Counter />, *)
     (*   document.getElementById('container') *)
     (* ); *)

let counter =
  Reactjs.make_class_spec
    ~initial_state:(fun ~this -> (object%js val count = 0 end))
    (fun ~this ->
       Reactjs.DOM.make
         ~elem_spec:(fun ~this ->
             (object%js
               val onClick =
                 this##setState (object%js val count = this##.state##.count + 1 end)
               val className = Js.string "Some Name"
             end)
           )
         ~tag:`button
         (`Text_nodes [Printf.sprintf "Click me, number of clicks: %d" this##.state##.count])
    )
  |> Reactjs.create_class

let () =
  Reactjs.(render (create_element_from_class counter) (Reactjs.get_elem ~id:"container"))

