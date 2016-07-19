let commentBox =
  Reactjs.react##createClass
    (object%js(self)
      val displayName = Js.string "CommentBox"
      method render =
        Reactjs.react##createElement_withPropsAndSingleText
          (Js.string "div")
          (object%js val className = Js.string "commentBox" end)
          (Js.string "Hello, world! I am a CommentBox.")
      method getInitialState = Js.null
      method getDefaultProps = Js.null
    end)

let () =
  Reactjs.reactDOM##render
    (Reactjs.react##createElement_WithReactClass commentBox Js.null)
    (Dom_html.getElementById "content")
