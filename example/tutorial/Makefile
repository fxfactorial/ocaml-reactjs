
all:tutorial.ml
	ocamlfind ocamlc -package react_js -linkpkg tutorial.ml
	js_of_ocaml a.out -o tutorial.js
	rm -f a.out

clean:
	rm -rf *.cmo *.cmt *.cmi tutorial.js
