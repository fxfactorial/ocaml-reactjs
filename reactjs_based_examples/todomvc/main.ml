type task =
  {
    label: string;
    completed: bool;
  }

type tab = All | Active | Completed
  
type state = {
  tasks: task list;
  tab: tab;
}

let initial_state =
  let tasks = [{label = "Initial task";
                completed = false};
               {label = "Completed task";
                completed = true};
               {label = "Final task";
                completed = true};]
  in
  {tasks = tasks;
   tab = Completed}
                
let todo_item (task : task) =
  let open Reactjs in
  let open Infix in
  make_class_spec
    (fun ~this ->
       ignore(this);
       DOM.make ~tag:`li
         [Elem (DOM.make 
                  ~tag:`div
                  ~elem_spec:(object%js
                    val className = "view"
                  end)
                  [Elem (DOM.make ~tag:`input ~elem_spec:
                           ([("type", Js.string "checkbox");
                             ("checked", Js.string (if task.completed then "checked" else ""));
                             ("className", Js.string "toggle")]
                            >>> object%js end)
                           []);
                   Elem (DOM.make 
                           ~tag:`label
                           [Text task.label;]);
                   Elem (DOM.make 
                           ~tag:`button
                           ~elem_spec:(object%js
                             val className = !*"destroy"
                           end)
                           [])])])
  |> create_class

let todo_input () =
  let open Reactjs in
  let open Infix in
  make_class_spec
    (fun ~this ->
       ignore(this);
       DOM.make ~tag:`input
         ~elem_spec:(object%js
           val className = !*"new-todo"
           val placeholder = !*"What needs to be done"
           val autofocus = !*"true"
         end)
         [])
  |> create_class

let root app =
  let open Reactjs in
  let open Infix in
  let tasks = List.filter (fun task ->
      match app.tab with
      | All -> true
      | Active -> not task.completed
      | Completed -> task.completed
    ) app.tasks in
  make_class_spec
    (fun ~this ->
       ignore(this);
       DOM.make ~tag:`section
         ~elem_spec:(object%js
           val className = !*"todoapp"
         end)
         [Elem (DOM.make ~tag:`header
                  ~elem_spec:(object%js
                    val className = !*"header"
                  end)
                  [Elem (DOM.make ~tag:`h1 [Text "todos"]);
                   Elem (create_element_from_class (todo_input ()))]);
          Elem (DOM.make ~tag:`section
                  ~elem_spec:(object%js
                    val className = !*"main"
                  end) [Elem (DOM.make ~tag:`input
                                ~elem_spec:([("type", Js.string "checkbox");
                                             ("className", Js.string "toggle-all")]
                                            >>> object%js end) []);
                        Elem (DOM.make ~tag:`label
                                ~elem_spec:([("htmlFor", Js.string "toggle-all");]
                                            >>> object%js end)
                                [Text "Mark all as complete"]);
                        Elem (DOM.make ~tag:`ul
                                ~elem_spec:(object%js
                                  val className = !*"todo-list"
                                end)
                                (List.map (fun task -> Elem (create_element_from_class (todo_item task))) tasks))]);
          Elem (DOM.make ~tag:`footer
                  ~elem_spec:(object%js
                    val className = !*"footer"
                  end)
                  [Elem (DOM.make ~tag:`span ~elem_spec:(object%js
                           val className = !*"todo-count"
                         end) [Text (string_of_int (List.length tasks))]);
                   Elem (DOM.make ~tag:`ul ~elem_spec:(object%js
                           val className = !*"filters"
                         end) 
                           [Elem (DOM.make ~tag:`li [Elem (DOM.make ~tag:`a ~elem_spec:(object%js
                                                             val href = !*"#/"
                                                             val className = !*"selected"
                                                           end) [Text "All"])]);
                            Elem (DOM.make ~tag:`li [Elem (DOM.make ~tag:`a ~elem_spec:(object%js
                                                             val href = !*"#/active"
                                                             val className = !*""
                                                           end) [Text "Active"])]);
                            Elem (DOM.make ~tag:`li [Elem (DOM.make ~tag:`a ~elem_spec:(object%js
                                                             val href = !*"#/completed"
                                                             val className = !*""
                                                           end) [Text "Completed"])])]);
                   Elem (DOM.make ~tag:`button ~elem_spec:(object%js
                           val className = !*"clear-completed"
                         end) [Text "Clear completed"])
                  ]);
         ])
  |> create_class

let () = Reactjs.(
    render
      ~react_elem:(create_element_from_class (root initial_state))
      (get_elem ~id:"container")
  )
