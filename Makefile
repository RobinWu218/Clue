play:
	ocamlbuild -use-ocamlfind -pkg core,yojson,ANSITerminal,str,ppx_sexp_conv main.byte && ./main.byte

clean:
	ocamlbuild -clean
