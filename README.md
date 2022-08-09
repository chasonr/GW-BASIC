# Microsoft GW-BASIC Interpreter Source Code

This repo contains the source-code for Microsoft's GW-BASIC interpreter, as of 1983, ported to assemble with the JWASM assembler.

## Original README

[Original README]: README-original.md

## Information

This repo contains two Makefiles. Both are meant to be used with the Open Watcom WMAKE.

* makefile.mas assembles the files using Microsoft Macro Assembler 3.01. This version seems to be close to the one that Microsoft used to build the original code.
* makefile assembles the files using JWASM. FreeDOS offers JWASM as a package, and JWASM has MASM compatibility as a design goal. The MASM compatibility falls short in a few areas, and changes are needed to make the files assemble with JWASM.

The Open Watcom WLINK program links the files to form a non-functioning GW-BASIC.EXE.

The original code has a number of unresolved external symbols, for such things as graphics that were left to the vendor to customize. The source file stubs.asm contains stubs for the missing functions.

## License

All files within this repo are released under the [MIT (OSI) License]( https://en.wikipedia.org/wiki/MIT_License) as per the [LICENSE file](https://github.com/Microsoft/GW-BASIC/blob/master/LICENSE) stored in the root of this repo.

