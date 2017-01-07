OCB_FLAGS = -use-ocamlfind -use-menhir
OCB       = ocamlbuild $(OCB_FLAGS)

all: native byte

_build/%:
	$(OCB) $*

native:
	$(OCB) src/contriver.native

byte:
	$(OCB) src/contriver.byte

clean:
	$(OCB) -clean

.PHONY: all native byte clean
