build:
	dune build src
	dune build bin

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

playgui:
	OCAMLRUNPARAM=b dune exec bin/main_gui.exe

playterminal:
	OCAMLRUNPARAM=b dune exec bin/main_terminal.exe
	
clean:
	dune clean

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

loc : 
	dune clean
	cloc --by-file --include-lang=OCaml .