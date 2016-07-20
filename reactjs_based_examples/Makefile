pkgs := reactjs
# js_debug := --debug-info --no-inline \
# --pretty --linkall --source-map-inline --source-map \
# --disable=optcall --disable=share --disable=shortvar \
# --disable=deadcode --disable=genprim

js_debug := --enable=debuginfo --disable=inline \
--enable=pretty --source-map-inline --source-map \
--disable=shortvar

all: code

code:example.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js

clean:;@rm -f *.cmi *.cmo *.cmt a.out *.js code

.PHONY:clean code
