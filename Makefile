all:
	ocamlbuild simulacrum.native
clean:
	ocamlbuild -clean
.PHONY: all clean
