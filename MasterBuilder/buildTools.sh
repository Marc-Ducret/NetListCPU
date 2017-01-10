#!/bin/bash

cd ../EZSimulator/
ocamlbuild nls_compile.byte
cd ../SimulatorEnv/
./make