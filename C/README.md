# Building

Default makefile CC program:

    make

Using clang:

    CC=clang make

Using gcc:

    CC=gcc make

# Compilation Database

To build a [JSON Compilation Database](https://clang.llvm.org/docs/JSONCompilationDatabase.html)
using [Bear](https://github.com/rizsotto/Bear), run:

    make clean
    bear make

Bear can be installed on debian with `apt install bear`.

