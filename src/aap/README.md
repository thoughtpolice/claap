# AAP source code map

> _"It's dangerous to go alone! Take this."_ - Old Man

**NOTE**: The build system expects **ALL** source code to the processor to live
under the `AAP` directory, and the only source file in this directory to be
`Main.hs`, which runs the simulation driver, in order to run
tests/tools. `Main.hs` should be kept as simple as possible.

- (**Source Legend**: **SIM** = only used for CLaSH simulation, **SYN** =
  synthesized to HDL, **DIR** = directory containing a group of files.)

```
|-- AAP                         <- DIR: AAP source code.
|   |-- ALU.hs                  <- SYN: Arithmetic Logic Unit.
|   |-- Decoder.hs              <- SYN: Instruction decoder.
|   |-- DMem.hs                 <- SYN: Memory unit for program data.
|   |-- Execute.hs              <- SYN: Execution unit.
|   |-- Export                  <- DIR: Directory containing synthesizble export declarations.
|   |   `-- ICE40.hs            <- SYN: Export to ICE40 board.
|   |-- Fetch.hs                <- SYN: Instruction fetch (IMem + Decoder).
|   |-- IMem.hs                 <- SYN: Memory unit for instruction data.
|   |-- PC.hs                   <- SYN: Program counter utilities.
|   |-- RegFile.hs              <- SYN: Register file.
|   |-- State.hs                <- SYN: Types and state for CPU execution.
|   |-- Types.hs                <- SYN: The most fundamental, basic types.
|   |-- Sim                     <- DIR: CLaSH simulation code -- NOT synthesizable!
|   |   |-- Disassemble.hs      <- SIM: Disassembler module, build on Decoder module.
|   |   |-- Test.hs             <- SIM: Test harness for tool tests run in simulation mode.
|   |   `-- Test                <- DIR: Individual tool tests.
|   |       `-- Disassembler.hs <- SIM: Testbench for the disassembler.
|   |-- Test.hs                 <- SIM: Test harness for synthesizable tests run in simulation mode.
|   `-- Test                    <- DIR: Individual synthesizable test benches.
|       `-- Decoder.hs          <- SYN: Testbench for the 16-bit instruction decoder.
|-- Main.hs                     <- SIM: Simulation driver.
`-- README.md                   <- You are here.
```
