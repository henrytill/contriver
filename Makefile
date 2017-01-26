OCB_FLAGS = -use-ocamlfind -use-menhir
OCB       = ocamlbuild $(OCB_FLAGS)

all: native

parser:
	$(OCB) src/parser.mli

native: parser
	$(OCB) src/main.native

byte: parser
	$(OCB) src/main.byte

test: deps parser
	$(OCB) -I src test/parser_test.byte
	./parser_test.byte

deps:
	ocamlfind query alcotest || opam install alcotest

clean:
	$(OCB) -clean

.PHONY: all parser native byte test deps clean
