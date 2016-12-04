play:
	ocamlbuild -pkgs yojson,ANSITerminal,str main.byte && ./main.byte

clean:
	ocamlbuild -clean
