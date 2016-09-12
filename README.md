# CLAAP: An Altruistic Processor, implemented in CLaSH

[![Linux Build Status](https://img.shields.io/badge/Linux%20Build-NIH-orange.svg)](https://example.com)
[![BSD3](https://img.shields.io/badge/License-BSD-blue.svg)](https://en.wikipedia.org/wiki/BSD_License)
[![Haskell](https://img.shields.io/badge/Language-Haskell-yellowgreen.svg)](https://www.haskell.org)

_CLAAP_ ("**CL**aSH + **AAP**") is an FPGA implementation of the [AAP][],
a 16-bit Harvard architecture processor, using
the [CLaSH](http://www.clash-lang.org) language: a compiler from
Haskell to hardware description languages.

[AAP]: http://www.embecosm.com/2015/04/20/aap-a-reference-harvard-architecture-for-embedded-compiler-development/

#### Table of Contents

- [Features](#features)
- [Demo](#demo)
- [Building](#building)
  - [Supported boards and synthesis flows](#supported-boards-and-synthesis-flows)
  - [Environment setup](#environment-setup)
  - [Configuring the build](#configuring-the-build)
  - [Do one stupid thing first](#do-one-stupid-thing-first)
  - [Running the build](#running-the-build)
    - [Build artifacts](#build-artifacts)
    - [Cleaning up](#cleaning-up)
  - [Compiler toolchain](#compiler-toolchain)
  - [Running the testbench](#running-the-testbench)
  - [Uploading to your board](#uploading-to-your-board)
- [Performance, timing, utilization](#performance-timing-utilization)
  - [Performance notes](#performance-notes)
  - [Timing and utilization](#timing-and-utilization)
- [References](#references)
- [Join in](#join-in)
- [Authors](#authors)
- [License](#license)

# Features

Here's the basic elevator pitch:

- Full 16-bit CPU written in 100% bona-fide Haskell
  with [CLaSH](http://www.clash-lang.org), implementing the full AAP
  instruction set.
- Probably significantly worse than [picorv32][] in every comparable way.
- The resulting CPU can be compiled to an executable for simulation,
  (System)Verilog, ~~or VHDL~~.
- Full 16 and 32-bit instruction support (AAP uses RISC-V style
  instruction chaining to allow extension of instructions to arbitrary
  lengths).
- Completely synthesizable with the open
  source [IceStorm Flow][icestorm], for a fully open-source
  HDL-to-hardware toolchain.
- ~~Completely synthesized test bench, for post place-and-route testing.~~
  (TODO!)
- A fast, dependable little build system, written with [Shake][shake].
- Nice, well-commented source code.
- Should be easy to integrate into bigger designs.
- LLVM-based toolchain available to build working programs.
- ~~Integration with [yosys-smtbmc][] for verification.~~ (TODO!)
- Some extra tools included:
  - ~~Full Disassembler.~~
  - ~~Full Assembler.~~
- Seriously, [picorv32][] is probably much better instead.

[picorv32]: https://github.com/cliffordwolf/picorv32
[shake]: http://www.shakebuild.com
[yosys-smtbmc]: http://www.clifford.at/papers/2016/yosys-synth-formal

Naturally, CLAAP inherits all the features of AAP itself (from
the [AAP announcement][aap-ann]):

[aap-ann]: http://www.embecosm.com/2015/04/20/aap-a-reference-harvard-architecture-for-embedded-compiler-development/

- **16-bit RISC architecture**. The core design sticks to the RISC
  principles of 3-address register-to-register operation, a small
  number of operations and a simple to implement datapath.  The
  fundamental data type is the 16-bit integer.
- **Configurable number of registers**. Although 32/64-bit RISC
  architectures typically have 16 or more general purpose registers,
  small deeply embedded processors often have far fewer. This
  represents a significant compiler implementation challenge.  To
  allow exploration of this area, AAP can be configured with between 4
  and 64, 16-bit registers.
- **Harvard memory layout**. The basic architecture provides a 64k
  byte addressed data memory and a separate 16M word instruction
  memory.  By requiring more than 16-bits to address the instruction
  memory, the compiler writer can explore the challenge of pointers
  which are larger than the native integer type. Deeply embedded
  systems often have very small memories, particularly for data, so
  the size of memories can be configured.
- **Multiple address spaces**. Many architectures also provide more
  than two address spaces, often for special purposes.  For example a
  small EEPROM alongside Flash memory, or the Special Purpose Register
  block of OpenRISC.  AAP can support additional address spaces,
  allowing support for multiple address spaces throughout the tool
  chain to be explored.
- **24-bit program counter with 8-bit status register**. AAP requires
  a 24-bit program counter, which is held in a 32-bit register. The
  top bits of the program counter then form a status register.  Jump
  instructions ignore these top 8 bits. At present only one status bit
  is defined, a carry flag to allow multiple precision arithmetic.
- **16/32-bit instruction encoding**. A frequent feature of many
  architectures is to provide a subset of the most commonly used parts
  of the Instruction Set Architecutre (ISA) in a short encoding of
  16-bits.  Less common instructions are then encoded in
  32-bits. Optimizing to use these shorter instructions, is
  particularly important for compilers for embedded targets, where
  memory is at a premium. AAP provides such a 16-bit subset with a
  32-bit encoding of the full ISA. However it follows the instruction
  chaining of RISC-V, so even longer instructions could be created in
  the future.
- **Three address code**. AAP has stuck rigidly to the RISC principle
  of 3-address instructions throughout.  Almost all instructions come
  in two variants, one where the third argument is a register, and one
  where the third argument is a constant.
- **No flags for flow of control**. There are no flag registers
  indicating the results of operations for use in conditional jumps.
  Instead the operation is encoded within the jump instruction
  itself. There is an 8-bit status register as part of the program
  counter, which includes a carry flag.  However this is not used for
  flow-of-control, but to enable mutliple precision arithmetic.
- **Little endian**. The architecture is little-endian -- the least
  significant byte of a word or double word is at the lowest
  address. The behavior for instruction memory is that one word is
  fetched, since it may be a 16-bit instruction.  If a second word is
  needed, then its fields are paired with the first instructions to
  give larger values for each field.  This is done in little-endian
  fashion, i.e. the field from the second instruction forms the most
  significant bits of the combined field.
- **No delay slots**. Early RISC designs introduced the concept of a
  delay slot after branches. This avoided pipeline delays in branch
  processing.  Implementations can now avoid such pipeline delay, so
  like most modern architectures, AAP does not have delay slots.
- **NOP with argument for simulator control**. This idea is taken from
  OpenRISC.  The NOP opcode includes fields to specify a register and
  a constant.  These can be used in both hardware and simulation to
  trigger side-effects.

# Demo

- **Screencast showing off the build system**: Like a videocast, but you have to
  do a lot of reading.

  [![asciicast](https://asciinema.org/a/bjdyr19g4fvhxxzm3xt4dzk79.png)](https://asciinema.org/a/bjdyr19g4fvhxxzm3xt4dzk79)

- **Gifcast showing off IRL usage**: A videocast that loops -- forever.
  
  [![TODO FIXME](https://i.imgur.com/vPtJ1VV.jpg)](https://i.imgur.com/vPtJ1VV.jpg)

# Building

Building the design, running the tests and uploading to a board all
depends on the specific combination of tools and hardware you have
available.

(Simulation is always available in multiple forms, if you have no hardware
available. The open source [IceStorm Flow][icestorm] can be installed with no
hassle or hardware, allowing you to run and test the full build process.)

## Supported boards and synthesis flows

The following synthesis flows have been tested:

| Flow | Description | Supported | Notes |
| --- | --- | --- | --- |
| [IceStorm Flow][icestorm] | Open-source flow for iCE40 FPGAs | :white_check_mark: **Full** | Default flow and primary target. |
| iCEcube2/Diamond | Official Lattice flow for iCE40 FPGAs | :x: **No** | |
| WebPack ISE | Xilinx flow for Spartan FPGAs | :x: **No** | |
| Vivado | Xilinx flow for high-end Xilinx FPGAs | :x: **No** | |
| Quartus | Altera flow for Cyclone/etc FPGAs | :x: **No** | |

[icestorm]: http://www.clifford.at/icestorm/

The following matrix lists the set of supported hardware and synthesis
flows that have been tested and are supported by the build system, or
ones that I inevitably plan to try and support if possible (or if
others can confirm support):

- (**Flow Legend**: **ICE** = IceStorm Flow, **ISE** = WebPack ISE, **VIV** =
  Vivado, **QR** = Quartus.)
- (**Support Legend**: :white_check_mark: = Supported, :heavy_exclamation_mark:
  = Untested, but should work with some fiddling, :x: = Unsupported without a
  bit of work.)

| Board | Chip | Flow | Support | Notes |
| --- | --- | --- | --- | --- |
| [iCE40-HX8K Breakout Board][8kbb] | iCE40-HX8K-CT256 | **ICE** | :white_check_mark: | Default board; sitting on my desk. |
| [iCE40-HX1K "IceStick"][icestick] | iCE40-HX1K-VQ144 | **ICE** | :heavy_exclamation_mark: | |
| [IcoBoard][icoboard] | iCE40-HX8K-CT256(?) | **ICE** | :heavy_exclamation_mark: | |
| [Go Board][goboard] | iCE40-HX1K-VQ100 | **ICE** | :heavy_exclamation_mark: | |
| [iCEblink40][iceblink] | iCE40-HX1K-VQ100 | **ICE** | :heavy_exclamation_mark: | |
| [Papilio One 500k][pp1_500k] | Spartan 3E XC3S500E | **ISE**, **VIV** | :x: | Need to dust off from my desk. |
| [Terasic DE1][de1] | Cyclone II 2C20 | **QR** | :x: | No Altera kits on-hand. |

[icestick]: http://www.latticesemi.com/icestick
[8kbb]: http://www.latticesemi.com/Products/DevelopmentBoardsAndKits/iCE40HX8KBreakoutBoard.aspx
[icoboard]: http://www.icoboard.org/
[goboard]: https://www.nandland.com/goboard/introduction.html
[iceblink]: http://www.latticesemi.com/iceblink40-hx1k
[pp1_500k]: http://store.gadgetfactory.net/papilio-one-500k-spartan-3e-fpga-dev-board/
[de1]: http://www.terasic.com.tw/cgi-bin/page/archive.pl?No=83

## Environment setup

The build system and hardware design itself are both written in
Haskell, so you will need the Haskell and CLaSH compiler(s), on top of
the needed synthesis tools, and the testing tools.

For the hardware design and build system, you need:

  - GHC 7.10.3 (exactly)
  - Cabal 1.24 or above (it **must** be version 1.24 or later!)
  - The CLaSH compiler (via `cabal install clash-ghc`)

For synthesis, you need:

  - icestorm -- <http://www.clifford.at/icestorm/>
  - yosys -- <http://www.clifford.at/yosys/>
  - arachne-pnr -- <https://github.com/cseed/arachne-pnr>

For running the full testbench, you also need:

  - Icarus Verilog -- <http://iverilog.icarus.com/>
  - Verilator -- <http://www.veripool.org/wiki/verilator>

## Configuring the build

Look inside `cfg/build.cfg`, which is extensively documented with
configuration options for the build system. These values are primarily
used to override the locations to any needed tools, and configuring
the synthesis and upload process (e.g. you must specify what board you
plan on building for).

Read the options in `cfg/build.cfg` for more. The default settings are
appropriate for synthesis onto
the [iCE40-HX8K Breakout Board][default-board].

Note that the build system is very smart: if you modify
`cfg/build.cfg`, by changing some value for example, the build system
will automatically detect this, and re-run the affected rules that are
touched by that option.

[default-board]: http://www.latticesemi.com/Products/DevelopmentBoardsAndKits/iCE40HX8KBreakoutBoard.aspx

## Do one stupid thing first

Do this first, after you've run `cabal install clash-ghc`:

```
$ cd src/clash-ice40 && cabal install && cd ../..
```

This will install a utility library that is used by both the AAP
implementation, and the build system, when synthesizing designs for
Lattice iCE40 FPGAs. It needs to be installed into the user package
database directly, as the `clash` executable needs to be able to find
the `clash-ice40` package when it compiles the design.

The need to do this manually is a short-term problem. In the future,
the build system will do it automatically if needed (and further down
the line, hopefully integration and improvements to `cabal new-build`
will allow this to be transparent).

## Compiler toolchain

TODO FIXME

## Running the build

If you have all of the necessary prerequisites, the following might
work if you're lucky:

```
$ ./do -j # -j means "use as many processors as possible in parallel"
```

This will automatically compile the Shake build system when you run it
for the first time. This uses `cabal new-build` (which is why you need
`cabal-install` 1.24 or above). Note that this may take a while, since
`cabal` will have to download and install all the dependencies of the
build system. Afterwords, the build will start instantaneously.

You may modify any of the source code to the AAP implementation, and
rerun `./do`. It will always recompile what is necessary based on your
changes.

Furthermore, if you modify any of the build system source code, `./do`
will automatically re-compile the build system before continuing.

You may specify a target to be built directly, as an argument to
`./do`. The default target (if no arguments are specified) builds the
simulation executables, the FPGA bistream, and does a timing analysis.

You can generate a fancy `report.html` file detailing the steps the
build system took, with `./do --report`.

Run `./do --help` for more build system options.

### Build artifacts

The results of the build are under the `build/` directory. Some of
these results are:

  - Binary results
    - `build/claap.blif`: resulting design in BLIF netlist format, before
      place-and-route.
    - `build/claap.asc`: resulting design in IceStorm ASCII format, after
      synthesis and place-and-route.
    - `build/claap.bin`: FPGA binary, ready for upload.
  - Synthesis results
    - `build/claap-synth.v`: Single-file Verilog output from yosys, containing
      the entire design as an optimized Verilog module. This has the reset and
      clock attached to an onboard PLL, typically.
    - `build/claap-simple.v`: Single-file Verilog output from yosys, containing
      the entire design as an optimized Verilog module. The wires, etc are all
      free, so this can be incorporated into other designs easily.
  - Logs
    - `build/synth.log`: Synthesis log.
    - `build/pnr.log`: Place-and-route log.
    - `build/timing.log`: Timing analysis results.

### Cleaning up

```
$ ./do clean # or `rm -rf build/`
```

## Running the testbench

To run the full testbench:

```
$ ./do test
```

This will:

  - Run the CLaSH simulation, which is created by compiling the
    Haskell program to an ordinary executable. This will run several
    tests, and exercise a testbench that will also be compiled to
    Verilog.
  
  - Run a simulation of the CLaSH compiler output with `iverilog`. The
    Verilog source and Verilog testbench is generated by the CLaSH
    compiler automatically.
    
  - Run a simulation of the CLaSH compiler output with
    `verilator`. Like before, the Verilog source is generated by the
    CLaSH compiler automatically.

  - Run a timing analysis on the resulting design, and spit out the
    path delay and max frequency analysis results. If `cfg/build.cfg`
    has been set up to specify `ICETIME_CHECK_MHZ`, this will
    optionally ensure that the resulting design can meet the specified
    timing requirement in megahertz. If the design cannot meet the
    specified requirement, the test fails.

## Uploading to your board

If you've specified everything correctly, you can attempt to set your
house on fire by uploading the resulting design to your board, with
the appropriate programming tool:

```
$ ./do upload
```

Note that the above example assumes your user account has permission
(somehow) to upload the FPGA bitstream to the board. This typically
involves directly interfacing with a USB port device (requiring
`write(2)`/`open(2)` capabilities), so you may need `sudo` to upload.

For iCE40 boards, fixing this on Linux with `udev` is relatively
straightforward, allowing you to upload bitstream files with your
unprivileged user account. First, create a file
`/etc/udev/rules.d/99-fpga-icestorm.rules` with the contents:

```
ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0660", GROUP="plugdev"
```

Add yourself to the `plugdev` group. Log-out and log back in to reset
your running session to have the proper group permissions.

This will cause `udev` to automatically assign group-write permissions
to any Lattice iCE40 FPGA that's plugged in, allowing the `plugdev`
group to open/write to the device. This is specified as a custom user
rule for `udev`, with priority 99, meaning it will happen after all
other rules (taking priority in case of conflict).

With this in place you should be able to simply plug in your device
via USB and your unprivileged user should be able to upload bitstream
files.

# Performance, timing, utilization

TODO FIXME

## Performance notes

## Timing and utilization

# References

- [EAP 13: AAP instruction specification][eap13]
- [EAP 14: Verilog AAP implementation][eap14]

[eap13]: http://www.embecosm.com/appnotes/ean13/ean13.html
[eap14]: http://www.embecosm.com/appnotes/ean14/ean14.html

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/claap`

[contribute]: https://github.com/thoughtpolice/claap/blob/master/.github/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/claap/issues
[gh]: http://github.com/thoughtpolice/claap

# Authors

See
[AUTHORS.txt](https://github.com/thoughtpolice/claap/blob/master/AUTHORS.txt).

# License

BSD3. See
[LICENSE.txt](https://github.com/thoughtpolice/claap/blob/master/LICENSE.txt)
for the exact terms of copyright and redistribution.
