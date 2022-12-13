build:
	dune build src
	dune build bin
	dune build test

utop:
	OCAMLRUNPARAM=b dune utop src

tests:
	dune exec test/main.exe

playgui:
	OCAMLRUNPARAM=b dune exec gui/main_gui.exe

playterminal:
	OCAMLRUNPARAM=b dune exec bin/main_terminal.exe

zip:
	dune clean
	zip -r wordbites.zip . -x@exclude.lst
	
clean:
	dune clean
	rm -f wordbites.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

loc : 
	dune clean
	cloc --by-file --include-lang=OCaml .