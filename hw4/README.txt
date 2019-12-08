B06902021 吳聖福
B07902024 塗大為

Compile:
  $ make
(use `-j` for parallel compilation)

Usage:
  $ ./parser [file]

New features:
* Allow negative constant values (in scanner)
* Imitate GCC error messages (e.g. notes, code printing, colors, exit status)
  (note that we don't print message on success)

Implemented additional errors / warnings:
* [ERROR] Calling variables
* [ERROR] Negative / non-integer array size
* [ERROR] Assigning void values / functions
* [WARNING] Return with value in void function / return without value in non-void function
* [WARNING] Incompatible array dimensions / types in parameter passing
