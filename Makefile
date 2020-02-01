OCB_FLAGS    = -use-ocamlfind -use-menhir -classic-display
OCB          = ocamlbuild $(OCB_FLAGS)
OPAM_INSTALL = opam install -y

DEPENDENCIES = alcotest num result stdlib-shims llvm.6.0.0

all: main.native

test: tests.native
	./tests.native
	@echo
	@echo Cleaning up...
	rm -f average_main
	rm -f _build/examples/output.o

parser:
	$(OCB) src/parser.mli

main.native: parser
	$(OCB) src/$@

tests.native: parser
	$(OCB) -I src test/$@

deps:
	@which ocamlfind || $(OPAM_INSTALL) ocamlfind
	@which ocamlbuild || $(OPAM_INSTALL) ocamlbuild
	@which menhir || $(OPAM_INSTALL) menhir
	@ocamlfind query $(DEPENDENCIES) || $(OPAM_INSTALL) $(DEPENDENCIES)

clean:
	rm -f average_main
	$(OCB) -clean

.PHONY: all parser test deps clean
