OCB_FLAGS = -use-ocamlfind -use-menhir
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

parser:
	$(OCB) src/parser.mli

native: parser
	$(OCB) src/main.native

byte: parser
	$(OCB) src/main.byte

test: sanity parser
	$(OCB) -I src test/parser_test.byte
	ocamlrun parser_test.byte

sanity:
	ocamlfind query alcotest

clean:
	$(OCB) -clean

.PHONY: all parser native byte sanity clean
