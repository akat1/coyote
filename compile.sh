#! /bin/sh

FILE=$1
OUT=$2

echo "coyote..."
./coyote -i $FILE -o tmp.s
echo "nasm..."
nasm -felf -o tmp.o tmp.s
rm tmp.s
echo "linking..."
gcc -nostartfiles -o $OUT tmp.o stdlib/stdlib.o
rm tmp.o
