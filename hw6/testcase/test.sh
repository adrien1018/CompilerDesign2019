#!/usr/bin/env bash

if [ $# -lt 1 ]; then
  echo "Usage: $0 [source-c--] [answer-file] [parser-print(0/1)]"
  echo "Default: output=output.txt, parser-print=1"
  exit 1
fi

IN="$(realpath "$1")"
if [ "$2" != "" ]; then
  OUT="$(realpath "$2")"
else
  OUT="$(realpath output.txt)"
fi

cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

ASM=output.s
SOURCE="$(mktemp --tmpdir=. --suffix=.cpp)"
RES1="$(mktemp)"
RES2="$(mktemp)"
EXE="$(mktemp)"

make -j8 -C ..
if [ "$3" == '1' ]; then
  ../parser -o "$ASM" "$IN"
else
  ../parser -o "$ASM" "$IN" > /dev/null 2> /dev/null
fi
riscv64-linux-gnu-gcc -static -O0 -o "$EXE" main.S "$ASM"
\time qemu-riscv64 "$EXE" > "$RES1"

echo '#include "header.h"' > "$SOURCE"
cat "$IN" >> "$SOURCE"
riscv64-linux-gnu-gcc -static -O0 -o "$EXE" "$SOURCE"
\time qemu-riscv64 "$EXE" > "$RES2"

diff "$RES1" "$RES2"
cp "$RES2" "$OUT"
rm "$ASM" "$SOURCE" "$RES1" "$RES2" "$EXE"
