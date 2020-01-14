B06902021 吳聖福
B07902024 塗大為

Compile:
  $ make
(use `-j` for parallel compilation)
* Note that our execution & compilation environment is `linux1` workstation
  instead of the given VM, since we need to use a new version of g++/bison/flex.
  (There are `riscv64-linux-gnu-*` cross-compiler on workstations now.)

Usage:
  $ ./parser -o [output=output.s] [file] [-O]
* Use `-O` to turn on optimization

Notes:
* We use an external library `argparse` (https://github.com/p-ranav/argparse) to parse the command line arguments.

New features:
* Fix operator precedence
* Allow expression lists in `if`, `while`, `return`
* Allow long distance jumps in branch instructions (GCC and Clang both fail on this case)
* Register lifetime analysis
* Register frequency analysis: pseudo registers that appear more frequently have higher priority to be placed in the real registers.
