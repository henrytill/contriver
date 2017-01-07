OCB_FLAGS = -use-ocamlfind -use-menhir
OCB       = ocamlbuild $(OCB_FLAGS)

all: _build/parser.mli

_build/%:
	$(OCB) $*

clean:
	$(OCB) -clean

.PHONY: all clean
