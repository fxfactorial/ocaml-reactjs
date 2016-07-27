## Compile
`ocamlfind ocamlc -g -package reactjs -linkpkg main.ml -o code && js_of_ocaml --enable=debuginfo --disable=inline --enable=pretty  code -o code.js`
