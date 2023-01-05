.PHONY: all clean

all:
  ocamlbuild -use-ocamlfind try3.ml
  ocamlbuild -use-ocamlfind try5.ml

clean:
  ocamlbuild -clean