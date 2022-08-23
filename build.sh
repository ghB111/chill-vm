#!/usr/bin/env bash

mkdir ./build
ghc ./chill-run.hs ./Parser.hs  ./Vm.hs ./Compiler.hs  -o ./build/chill-run

