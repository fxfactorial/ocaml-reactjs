# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP
.PHONY: readme

# Add dir name here when example is ready
dirs := basic basic-click-counter quadratic todomvc
files := $(foreach dir,$(dirs),$(wildcard reactjs_based_examples/$(dir)/*.ml))

dist_clean:; @rm -f README.md

readme: dist_clean
	@cp static/README_base.markdown README.md
	@for file in ${files} ; do \
	    bash static/add_to_read_me.sh $$file ; \
	done

pdf_doc := reactjs_bindings.pdf

# Install wkhtmltopdf with brew cask install wkhtmltopdf
generate_pdf:doc
	@printf '\n\tCreating a PDF of the documentation\n'
	@(cd api.docdir; \
	wkhtmltopdf  --load-error-handling ignore * ${pdf_doc})
	@mv api.docdir/${pdf_doc} .
	@printf '\n\tCreated %s!\n' ${pdf_doc}
