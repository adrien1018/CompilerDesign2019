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

echo 'Compiling...'
make -j8 -C .. > /dev/null
if [ "$3" == '1' ]; then
  ../parser -o "$ASM" -O "$IN"
else
  ../parser -o "$ASM" -O "$IN" > /dev/null 2> /dev/null
fi
echo -n 'Ours: '
riscv64-linux-gnu-gcc -static -O0 -o "$EXE" main.S "$ASM"
\time -f 'User: %U s, RSS: %M KB' qemu-riscv64 "$EXE" > "$RES1"

echo -n 'GCC: '
echo '#include "header.h"' > "$SOURCE"
cat "$IN" >> "$SOURCE"
riscv64-linux-gnu-gcc -static -O0 -o "$EXE" "$SOURCE"
\time -f 'User: %U s, RSS: %M KB' qemu-riscv64 "$EXE" > "$RES2"

diff "$RES1" "$RES2"
if [ $? -eq 0 ]; then echo Correct; fi
cp "$RES2" "$OUT"
rm "$ASM" "$SOURCE" "$RES1" "$RES2" "$EXE"
