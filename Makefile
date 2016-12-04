play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

clean:
	ocamlbuild -clean
