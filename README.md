# tiger-compiler-ocaml

A compiler for the Tiger programming language and implemented in OCaml

## Preparing the environment for the project in Ubuntu >= 18.04

```
$ sudo apt update
$ sudo apt install opam
$ opam switch create 4.11.0
$ eval $(opam env)
$ opam install dune ppx_import ppx_deriving menhir ocamllex ocaml-lsp-server ounit2
```

## How to clean

```
$ dune clean
```

## How to compile

```
$ dune build
```

## How to run

```
$ dune exec driver
$ echo "1+1" | dune exec mytiger
```

## How to test the compiler

```
$ dune test -f
```

## References

 - [Recipes for OCamlLex](https://medium.com/@huund/recipes-for-ocamllex-bb4efa0afe53)
 - [Dune doc](https://dune.readthedocs.io/en/stable/index.html)
 - [ocamlc options](https://ocaml.jp/refman/ch08s02.html)
 - [Lexing modules](https://ocaml.jp/?Lexing)
 - [OUnit2](https://github.com/gildor478/ounit)
