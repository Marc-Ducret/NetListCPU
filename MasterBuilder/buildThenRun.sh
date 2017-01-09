#!/bin/bash

./buildTools.sh
./buildProc.sh $1
./runProc.sh $2 $3