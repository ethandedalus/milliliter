# Revision history for milliliter

## 0.1.0.0 -- 2026-03-27

* Bootstraped barebones lexer and initialized tests.
* Added support for token spans and add more lexer rules
* Add a `Justfile`
* Remove `__testdata__` from git because it's not necessary for now

## 0.1.1.0 -- 2026-03-29

* Add test cases for lexer and parser
* Handle whitespace better in lexer
* Create parser module and write parser combinators
* Create an ebnf file that will is intended to be updated as parser combinators are changed
* Add `Justfile` commands
* Start codegen module

## 0.1.2.0 -- 2026-03-31

* Add support in the parser for unary negation and complement
* Add tests for unary operations in the parser test suite
* Write a compiler driver (`x.py`) which wraps the compiler. It invokes `gcc` for preprocessing, assembling, and linking, but calls `milliliter` to perform the actual compilation.

## 0.1.3.0 -- 2026-04-04

* Implement compiler passes to replace pseudoregisters and fix up invalid instructions
* Add test cases for codegen of ASM AST and emission
* Refactor the code emission stage
* Add a custom `.ghci` for easier repl dev environment configuration
* Clean up compiler driver

## 0.1.4.0 -- 2026-04-09

* Implement support for the following binary operators: `+`, `-`, `*`, `/`, and `%`
* Restructure the compiler so that stages are composable via `>=>`, making for much easier testing
* Refactor compiler errors

## 0.1.5.0 -- 2026-04-16

* Implement support for bitwise operators
* Allow for local variables
* Implement postfix and prefix increment and decrement as well as compound assignment for all arithmetic and bitwise operators


