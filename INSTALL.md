# Installation Instructions

## Build/Run
To build the project, type in terminal of directory folder
```
make build
```

To run the project in terminal:
```
make playterminal
```

To run the project with OCaml graphics:
```
make playgui
```

To run the gui, you might have to run
```
dune build
```

## Packages to Install
Install OCaml with all the default packages with this guide

https://cs3110.github.io/textbook/chapters/preface/install.html

Additional packages that have to be installed
```
opam install ANSITerminal
opam install graphics
```


Packages we are using:
- `ounit2`: This testing tool was used extensively throughout CS3110, so it should be installed
- `str`: Comes apart of the OCaml basic library
- `ANSITerminal`: Addition terminal commands that we will use for the text-based version of the project
- `graphics`: Used to display graphics in a window and allows user interaction with the game