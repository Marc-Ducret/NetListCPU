#!/bin/bash

./../minijazz/mjc.byte src/$1.mj
cp src/$1.net genSrc/$1.net
rm src/$1.net
./../EZSimulator/_build/nls_compile.byte genSrc/$1.net
cp genSrc/$1.net.c genSrc/$1.c
rm genSrc/$1.net.c
cp src/*.c genSrc/
gcc genSrc/$1.c -o bin/proc.exe