build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

playterminal:
	OCAMLRUNPARAM=b dune exec bin/main_terminal.exe
	
clean:
	dune clean

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

loc : 
	cloc --by-file --include-lang=OCaml .