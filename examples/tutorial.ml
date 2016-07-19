let commentBox =
  Reactjs.react##createClass
    (object%js
      val displayName = Js.string "CommentBox" |> Js.Opt.return
      method render =
        Js.wrap_meth_callback
          (fun this_handle ->
             Firebug.console##log this_handle;
             Reactjs.react##createElement_withString (Js.string "div")
          )
    end)

let () =
  Reactjs.reactDOM##render
    (Reactjs.react##createElement_WithReactClass commentBox Js.null)
    (Dom_html.getElementById "content")
