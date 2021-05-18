# hack-assembler
A Hack assembler in OCaml. A program that translates assembly code for the Hack computer into its binary representation.

This is project 6 of the [Nand to Tetris course](https://www.nand2tetris.org/).

To run the program, run the following

``` shell
dune exec -- ./assembler.exe <filename>.asm
```
where `<filename>.asm` is the assembly file you wish to translate. The file `<filename>.hack` should be created.

## Parsing/Lexing

The parsing library menhir, and the lexing library ocamllex, are used to extract the A-instructions, C-instructions, and Labels. It's probably overkill, but it was quite simple to setup after following chapter 10 of [this book about functional programming in OCaml](https://www.cs.cornell.edu/courses/cs3110/2021sp/textbook/interp/intro.html).

## Dependencies

```shell
opam install base core menhir stdio
```
