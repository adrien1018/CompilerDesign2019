B06902021 吳聖福
B07902024 塗大為

Compile:
  $ make
(use `-j` for parallel compilation)

Usage:
  $ ./parser [file]

Notes:
* The entrance function is declared as `main` instead of `MAIN` to pass the
  example testcases. The generated label is still `_start_MAIN`.

Implementation details:
* The code generation is split into two components, the IR instruction
  generator and the RISC-V instruction generator. The instructions produced by
  the IR generator operates on unlimited "pseudo-registers", and the RISC-V
  generator maps pseudo-registers into real registers or stack.
* When the real registers run out, load/store operations must be performed.
  To minimize stack usage, each pseudo-register is associated with its offset
  to the stack pointer (or not being allocated yet) and whether it's dirty.
  The store operation is performed only when the pseudo register is dirty
  (being modified after loaded). Clean registers have higher priorities to be
  assigned to the pseudo registers.
* There will only be one epilogue for each function declaration. The return
  statement is translated into two steps:
  - move the return value into `a0`
  - jump to the epilogue of the function
* Calling convention:
  - the local arrays allocated by the IR generator are refernces by
    `sp + offset`.
  - the stack storage allocated for the pseudo registers are referenced by
    `fp - offset`.
  - the (used) callee-saved registers are stored in
    `sp + [local] + offset`.
  - the (used) caller-saved registers are stored in
    `sp + [local] + [callee-saved] + offset`.

New features / modifications:
* Supports unconditional jumps of distances > 2^19.

Implemented additional errors / warnings (in comparison with hw4):
* [ERROR] Non-const initialization of global variables.
* [ERROR] Declarations of void arrays.
* [ERROR] Passing strings to scalar arguments.
* [ERROR] Declaring functions that return arrays.
