#!/bin/sh

ocamlbuild -lflags -cc,g++ -libs llvm,llvm_bitwriter,llvm_analysis compiler.byte
