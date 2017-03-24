OCB_FLAGS    = -use-ocamlfind -use-menhir
OCB          = ocamlbuild $(OCB_FLAGS)
OPAM_INSTALL = opam install -y

DEPENDENCIES = alcotest

all: native

parser:
	$(OCB) src/parser.mli

native: parser
	$(OCB) src/main.native

byte: parser
	$(OCB) src/main.byte

test: parser
	$(OCB) -I src -I test test/tests.byte
	./tests.byte

deps:
	@which ocamlfind || $(OPAM_INSTALL) ocamlfind
	@which ocamlbuild || $(OPAM_INSTALL) ocamlbuild
	@which menhir || $(OPAM_INSTALL) menhir
	@ocamlfind query $(DEPENDENCIES) || $(OPAM_INSTALL) $(DEPENDENCIES)

clean:
	$(OCB) -clean

.PHONY: all parser native byte test deps clean
