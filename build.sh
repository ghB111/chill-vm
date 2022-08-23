#!/usr/bin/env bash

mkdir -p ./build
ghc ./chill-run.hs ./Parser.hs  ./Vm.hs ./Compiler.hs  -o ./build/chill-run

