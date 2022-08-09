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

## Tags

The "MASM" tag points to a version of the source that assembles and links with MASM 3.01, and has the fewest changes possible from the original to meet that goal.

The "JWASM" tag points to a version of the source that assembles and links with JWASM v2.12pre as supplied with FreeDOS 1.3. When assembled with MASM, it builds the same binary as the MASM tag.

JWASM builds a slightly different binary than MASM. The main reason is that JWASM is much more aggressive at optimizing JMP instructions. MASM will sometimes allocate three bytes for a JMP, and then use the two-byte JMP and a NOP. JWASM, in this situation, will delete the NOP and generate only the two-byte JMP.

## License

All files within this repo are released under the [MIT (OSI) License]( https://en.wikipedia.org/wiki/MIT_License) as per the [LICENSE file](https://github.com/Microsoft/GW-BASIC/blob/master/LICENSE) stored in the root of this repo.

