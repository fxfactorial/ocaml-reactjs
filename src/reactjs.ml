let (__react, __reactDOM, __reactDOMServer) :
  'a Js.t * 'a Js.t * 'a Js.t =
  let open Js.Unsafe in
  let undef = Js.Unsafe.eval_string "undefined" in
  let require_module s =
    fun_call (js_expr "require") [|inject (Js.string s)|]
  in
  try
    (* Need to keep it this way, otherwise jsoo will optimize it
       out *)
    Js.typeof (eval_string "window") = Js.string "undefined";
    (* In Browser *)
    global##.React,
    global##.ReactDOM,
    undef
  with Js.Error _ ->
    (* In Node *)
    (try require_module "react" with _ -> undef),
    (try require_module "react-dom" with _ -> undef),
    (try require_module "react-dom-server" with _ -> undef)

class type react_dom_server = object
  method renderToString :
    react_element Js.t -> Js.js_string Js.t Js.meth
  method renderToStaticMarkup :
    react_element Js.t -> Js.js_string Js.t Js.meth
end

and react_dom = object
  method render :
    react_element Js.t -> #Dom_html.element Js.t -> unit Js.meth
  (* method render_WithCallback : *)
  (*   react_element Js.t -> #Dom_html.element Js.t -> unit Js.meth *)

  method unmountComponentAtNode :
    #Dom_html.element Js.t -> bool Js.t Js.meth

  method findDOMNode :
    react_component Js.t -> #Dom_html.element Js.t Js.meth
end

and react = object
  (* method _Children : <map: *)
  (*                       forEach *)
  (*                         count *)
  (*                         toArray *)
  (*                        only> *)
  (* method _Component *)
  method createElement_withString :
    Js.js_string Js.t -> react_element Js.t Js.meth

  method createElement_WithReactClass :
    react_class Js.t -> _ Js.Opt.t -> react_element Js.t Js.meth
  method cloneElement : react_element Js.t -> react_element Js.t Js.meth
  method isValidElement : 'a Js.t -> bool Js.t Js.meth
  (* method _PropTypes *)
  method createClass :
    <displayName : Js.js_string Js.t Js.Opt.t  Js.readonly_prop;
     render : ('a, react_element Js.t) Js.meth_callback Js.meth > Js.t ->
    react_class Js.t Js.meth
  method createFactory_withString :
    Js.js_string Js.t -> factory_function Js.t Js.meth
  (* method createMixin *)
    (* method _DOM a: [Function: bound ], *)
     (* abbr: [Function: bound ], *)
     (* address: [Function: bound ], *)
     (* area: [Function: bound ], *)
     (* article: [Function: bound ], *)
     (* aside: [Function: bound ], *)
     (* audio: [Function: bound ], *)
     (* b: [Function: bound ], *)
     (* base: [Function: bound ], *)
     (* bdi: [Function: bound ], *)
     (* bdo: [Function: bound ], *)
     (* big: [Function: bound ], *)
     (* blockquote: [Function: bound ], *)
     (* body: [Function: bound ], *)
     (* br: [Function: bound ], *)
     (* button: [Function: bound ], *)
     (* canvas: [Function: bound ], *)
     (* caption: [Function: bound ], *)
     (* cite: [Function: bound ], *)
     (* code: [Function: bound ], *)
     (* col: [Function: bound ], *)
     (* colgroup: [Function: bound ], *)
     (* data: [Function: bound ], *)
     (* datalist: [Function: bound ], *)
     (* dd: [Function: bound ], *)
     (* del: [Function: bound ], *)
     (* details: [Function: bound ], *)
     (* dfn: [Function: bound ], *)
     (* dialog: [Function: bound ], *)
     (* div: [Function: bound ], *)
     (* dl: [Function: bound ], *)
     (* dt: [Function: bound ], *)
     (* em: [Function: bound ], *)
     (* embed: [Function: bound ], *)
     (* fieldset: [Function: bound ], *)
     (* figcaption: [Function: bound ], *)
     (* figure: [Function: bound ], *)
     (* footer: [Function: bound ], *)
     (* form: [Function: bound ], *)
     (* h1: [Function: bound ], *)
     (* h2: [Function: bound ], *)
     (* h3: [Function: bound ], *)
     (* h4: [Function: bound ], *)
     (* h5: [Function: bound ], *)
     (* h6: [Function: bound ], *)
     (* head: [Function: bound ], *)
     (* header: [Function: bound ], *)
     (* hgroup: [Function: bound ], *)
     (* hr: [Function: bound ], *)
     (* html: [Function: bound ], *)
     (* i: [Function: bound ], *)
     (* iframe: [Function: bound ], *)
     (* img: [Function: bound ], *)
     (* input: [Function: bound ], *)
     (* ins: [Function: bound ], *)
     (* kbd: [Function: bound ], *)
     (* keygen: [Function: bound ], *)
     (* label: [Function: bound ], *)
     (* legend: [Function: bound ], *)
     (* li: [Function: bound ], *)
     (* link: [Function: bound ], *)
     (* main: [Function: bound ], *)
     (* map: [Function: bound ], *)
     (* mark: [Function: bound ], *)
     (* menu: [Function: bound ], *)
     (* menuitem: [Function: bound ], *)
     (* meta: [Function: bound ], *)
     (* meter: [Function: bound ], *)
     (* nav: [Function: bound ], *)
     (* noscript: [Function: bound ], *)
     (* object: [Function: bound ], *)
     (* ol: [Function: bound ], *)
     (* optgroup: [Function: bound ], *)
     (* option: [Function: bound ], *)
     (* output: [Function: bound ], *)
     (* p: [Function: bound ], *)
     (* param: [Function: bound ], *)
     (* picture: [Function: bound ], *)
     (* pre: [Function: bound ], *)
     (* progress: [Function: bound ], *)
     (* q: [Function: bound ], *)
     (* rp: [Function: bound ], *)
     (* rt: [Function: bound ], *)
     (* ruby: [Function: bound ], *)
     (* s: [Function: bound ], *)
     (* samp: [Function: bound ], *)
     (* script: [Function: bound ], *)
     (* section: [Function: bound ], *)
     (* select: [Function: bound ], *)
     (* small: [Function: bound ], *)
     (* source: [Function: bound ], *)
     (* span: [Function: bound ], *)
     (* strong: [Function: bound ], *)
     (* style: [Function: bound ], *)
     (* sub: [Function: bound ], *)
     (* summary: [Function: bound ], *)
     (* sup: [Function: bound ], *)
     (* table: [Function: bound ], *)
     (* tbody: [Function: bound ], *)
     (* td: [Function: bound ], *)
     (* textarea: [Function: bound ], *)
     (* tfoot: [Function: bound ], *)
     (* th: [Function: bound ], *)
     (* thead: [Function: bound ], *)
     (* time: [Function: bound ], *)
     (* title: [Function: bound ], *)
     (* tr: [Function: bound ], *)
     (* track: [Function: bound ], *)
     (* u: [Function: bound ], *)
     (* ul: [Function: bound ], *)
     (* var: [Function: bound ], *)
     (* video: [Function: bound ], *)
     (* wbr: [Function: bound ], *)
     (* circle: [Function: bound ], *)
     (* clipPath: [Function: bound ], *)
     (* defs: [Function: bound ], *)
     (* ellipse: [Function: bound ], *)
     (* g: [Function: bound ], *)
     (* image: [Function: bound ], *)
     (* line: [Function: bound ], *)
     (* linearGradient: [Function: bound ], *)
     (* mask: [Function: bound ], *)
     (* path: [Function: bound ], *)
     (* pattern: [Function: bound ], *)
     (* polygon: [Function: bound ], *)
     (* polyline: [Function: bound ], *)
     (* radialGradient: [Function: bound ], *)
     (* rect: [Function: bound ], *)
     (* stop: [Function: bound ], *)
     (* svg: [Function: bound ], *)
     (* text: [Function: bound ], *)
     (*       tspan: [Function: bound ] *)
    method version : Js.js_string Js.t Js.readonly_prop
        (* method __spread *)
end

and react_element = object

end

and react_component = object

end

and react_class = object

end

and factory_function = object

end

let react : react Js.t = __react

let reactDOM : react_dom Js.t = __reactDOM

(* Only makes sense on the server, hence the unit *)
let reactDOMServer : unit -> react_dom_server Js.t = fun () -> __reactDOMServer
