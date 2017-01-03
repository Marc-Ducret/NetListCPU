#!/bin/bash

minijazz="./minijazz/mjc.byte"
file=$1
simulateur="./Netlist simulator/nls.native"

"$minijazz"   "$file.mj"
"$simulateur" "$file.net"
