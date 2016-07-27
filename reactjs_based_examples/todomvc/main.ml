type task_id = int

type task =
  {
    id: task_id;
    label: string;
    completed: bool;
  }

type tab = All | Active | Completed
  
type state = {
  editing: task_id option;
  tasks: task list;
  tab: tab;
}

type history =
  state list

let initial_state =
  let tasks = [{id = 0;
                label = "Initial task";
                completed = false};
               {id = 1;
                label = "Completed task";
                completed = true};
               {id = 2;
                label = "Final task";
                completed = true};]
  in
  ref {editing = None;
       tasks = tasks;
       tab = All}

let (histories : history ref) =
  ref []

let observe _old_state new_state =
  Printf.printf "Mutation observed: %d\n" (List.length new_state.tasks)
  
let observer =
  ref (fun _old_state _new_state -> Printf.printf "Placeholder observer used")
           
let swap (ref : state ref) f =
  let old_state = !ref in
  ref := f ref;
  let new_state = !ref in
  ignore(!observer old_state new_state);
  histories := List.append [new_state] !histories;
  Printf.printf "History count: %d\n" (List.length !histories);
  ()

let set_tab state tab =
  {state with tab = tab}

let random_el arr =
    let n = Random.int (Array.length arr) in
    Array.get arr n;;

let colors =
  [|"blue"; "green"; "pink"; "purple"; "white"; "gray"|]

let random_color () =
  random_el colors

let todo_item (task : task) =
  let open Reactjs in
  let open Infix in
  make_class_spec
    (fun ~this ->
       ignore(this);
       DOM.make ~tag:`li
         ~elem_spec:(object%js
           val key = !*("li-todo-input-" ^ (string_of_int task.id))
       end)
         (if (match !initial_state.editing with
              | None -> false
              | Some id -> id = task.id) then
            [Elem (DOM.make ~tag:`input ~elem_spec:(object%js
                     val key = !*("todo-input-" ^ (string_of_int task.id))
                     val type_ = !*"text"
                     val style = (object%js val display = "block" val backgroundColor = !*("white") end)
                     val defaultValue = task.label
                     val onChange = (fun event ->
                         this##setState event##.target##.value |> Js.to_string
                         (* swap initial_state (fun state -> *)
                         (* let new_tasks = List.map (fun t -> *)
                         (*     if t.id = task.id then *)
                         (*       {t with label = event##.target##.value |> Js.to_string } *)
                         (*     else *)
                         (*       t) !state.tasks in *)
                         (* {!state with tasks = new_tasks}) *))
                     val onKeyUp = (fun event ->
                         Printf.printf "Key: %d\n" event##.which;
                         match event##.which with
                         | 13 -> swap initial_state (fun state -> 
                         let new_tasks = List.map (fun t ->
                             if t.id = task.id then
                               {t with label = event##.target##.value |> Js.to_string }
                             else
                               t) !state.tasks in
                           let _tt = List.find (fun t -> t.id = task.id) new_tasks in
                           Printf.printf "Task label after updating: %s\n" _tt.label;
                           {!state with 
                            tasks = new_tasks;
                            editing = None})
                         | 27 -> swap initial_state (fun state -> {!state with editing = None})
                         | _ ->  () 

                       )
                     val className = Js.string "edit"
                   end)
                     [])]
          else 
            [Elem (DOM.make 
                     ~tag:`div
                     ~elem_spec:(object%js
                       val className = "view"
                       val onDoubleClick = (fun _ -> swap initial_state (fun state ->
                           Printf.printf "Now editing %d\n"task.id;
                           {!state with editing = Some task.id}
                         ))
                     end)
                     [Elem (DOM.make ~tag:`input ~elem_spec:(object%js
                              val type_ = !*"checkbox"
                              val onClick = (fun _ -> swap initial_state (fun state ->
                                  let new_tasks = List.map (fun t -> 
                                      if t.id = task.id then
                                        {t with completed = not t.completed}
                                      else
                                        t
                                    ) !state.tasks in
                                  {!state with tasks = new_tasks}
                                ))
                              val checked = Js.string (if task.completed then "checked" else "")
                              val className = Js.string "toggle"
                            end)
                              []);
                      Elem (DOM.make 
                              ~tag:`label
                              [Text ((match !initial_state.editing with
                                   | None -> ""
                                   | Some editing -> if editing = task.id then "Editing: " else "") ^  task.label);]);
                      Elem (DOM.make 
                              ~tag:`button
                              ~elem_spec:(object%js
                                val onClick = (fun _ -> swap initial_state (fun state ->
                                    let new_tasks = List.filter (fun t -> 
                                        t.id != task.id
                                      ) !state.tasks in
                                    {!state with tasks = new_tasks}
                                  ))
                                val className = !*"destroy"
                              end)
                              [])])]))
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
           val onKeyDown = (fun event ->
               match event##.which with
               | 13 -> swap initial_state (fun state ->
                   let id = Random.int 64000 in
                   Printf.printf "Updating new task...\n";
                   Firebug.console##log event##.target##.value;
                   let new_tasks = List.append !state.tasks [{id = id; label = event##.target##.value |> Js.to_string; completed = false}] in
                   Printf.printf "New tasks updated\n";
                   {!state with tasks = new_tasks}
                 )
               | _ -> ())

         end)
         [])
  |> create_class

let root app =
  let open Reactjs in
  let open Infix in
  let tasks = List.filter (fun task ->
      Printf.printf "Checking if task matches: %d\n" task.id;
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
                                                             val onClick = (fun _ -> swap initial_state (fun state -> set_tab !state All))
                                                             val className = (match app.tab with
                                                                 | All -> !*"selected"
                                                                 | _ -> !*"")
                                                           end) [Text "All"])]);
                            Elem (DOM.make ~tag:`li [Elem (DOM.make ~tag:`a ~elem_spec:(object%js
                                                             val href = !*"#/active"
                                                             val onClick = (fun _ -> swap initial_state (fun state -> set_tab !state Active))
                                                             val className = (match app.tab with
                                                                 | Active -> !*"selected"
                                                                 | _ -> !*"")
                                                           end) [Text "Active"])]);
                            Elem (DOM.make ~tag:`li [Elem (DOM.make ~tag:`a ~elem_spec:(object%js
                                                             val href = !*"#/completed"
                                                             val onClick = (fun _ -> swap initial_state (fun state -> set_tab !state Completed))
                                                             val className = (match app.tab with
                                                                 | Completed -> !*"selected"
                                                                 | _ -> !*"")
                                                           end) [Text "Completed"])])]);
                   Elem (DOM.make ~tag:`button ~elem_spec:(object%js
                           val className = !*"clear-completed"
                           val onClick = (fun _ -> swap initial_state (fun state ->
                               let new_tasks = List.filter (fun t -> 
                                   not t.completed
                                 ) !state.tasks in
                               {!state with tasks = new_tasks}
                             ))
                         end) [Text "Clear completed"])
                  ]);
         ])
  |> create_class

let dump_state _ =
  List.iter (fun task -> print_endline (task.label ^ " [" ^ (string_of_bool task.completed) ^ "]")) !initial_state.tasks

let () =
  Js.Unsafe.global##.dump_state := dump_state

let () =
  Js.Unsafe.global##.get_state := (fun _ -> !initial_state)

let first_render =
  ref true

let wrap_root state_ref =
  let open Reactjs in
  let open Infix in
  make_class_spec
    (fun ~this ->
       ignore(this);
       DOM.make ~tag:`div
         ~elem_spec:(object%js
           val className = !*"overreact"
         end)
         [Elem (create_element_from_class (root !state_ref));
         ])
  |> create_class

let render_root state =
  let open Reactjs in
  (match !state.tab with
   | All -> Printf.printf "Current tab: All\n"
   | Active -> Printf.printf "Current tab: Active\n"
   | Completed -> Printf.printf "Current tab: Completed\n");
  (match !first_render with
   | _ -> let root_el_ = render
                 ~react_elem:(create_element_from_class (wrap_root state))
                 (get_elem ~id:"container") in
     first_render := false;
     Js.Unsafe.global##.example := root_el_;
   (* | false -> Js.Unsafe.global##.example##forceUpdate *)
  );
  Firebug.console##log Js.Unsafe.global##.example;
  ()

let replay_history =
  (fun _ ->
      let rec render_next_state = (fun states ->
          match (List.length states) with
          | 0 -> ()
          | _ -> let next_state = List.hd states in
            render_root (ref next_state);
            ignore(Dom_html.window##setTimeout (Js.wrap_callback (fun () -> render_next_state (List.tl states); ())) 500.);
        ()) in
      render_next_state (List.rev !histories)
    )

let () =
  Js.Unsafe.global##.replayHistory := replay_history;
  let app_observer = (fun (_old_state : state) (_new_state : state) -> render_root initial_state) in
  observer := app_observer;
  swap initial_state (fun state -> state := {!state with tab = All}; !state);
