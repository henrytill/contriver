OCB_FLAGS    = -use-ocamlfind -use-menhir -classic-display
OCB          = ocamlbuild $(OCB_FLAGS)
OPAM_INSTALL = opam install -y

DEPENDENCIES = alcotest result llvm.4.0.0

all: main.native

test: tests.native
	./tests.native

parser:
	$(OCB) src/parser.mli

main.native: parser
	$(OCB) src/$@

simple_emit.native:
	$(OCB) -I src examples/$@

_build/examples/output.o: simple_emit.native
	./$< $@

average_main: _build/examples/output.o examples/average_main.cpp
	clang++ examples/average_main.cpp $< -o $@

tests.native: parser src/* average_main
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
