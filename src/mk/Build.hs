module Main
  ( main -- :: IO ()
  ) where

import Control.Monad (void)
import Data.List (isInfixOf, isSuffixOf)

import Development.Shake.Fancy
import Development.Shake.FilePath

import Tools.Yosys
import Tools.Icestorm
import Tools.ArachnePNR
import Tools.IVerilog
import Tools.CLaSH
import Tools.Extra

import Options
import Config

bld :: FilePath -> FilePath
bld = ("build" </>)

main :: IO ()
main = shakeArgs myShakeOpts $ do
  -- Setup: a bunch of convenient aliases for all the outputs of the build
  let
    ice40pllv  = bld "ice40pll"     <.> "v"
    clashOut   = bld "clash-files"  <.> "txt"
    simulator  = bld "simulate"     <.> exe
    vsimulate  = bld "simulate"     <.> "vvp"
    yolog      = bld "synth"        <.> "log"
    netlist    = bld "claap"        <.> "blif"
    yoverilog  = bld "claap-synth"  <.> "v"
    yoverilog2 = bld "claap-simple" <.> "v"
    pnrout     = bld "claap"        <.> "asc"
    pnrlog     = bld "pnr"          <.> "log"
    fpgabin    = bld "claap"        <.> "bin"
    timings    = bld "timing"       <.> "log"

  -- Setup: Configuration file is currently hardcoded.
  usingConfigFile "cfg/build.cfg"

  -- Setup: Top level rules we want to run by default -- build the simulator,
  -- the specified image for uploading onto a board, the timing report, and also
  -- a full version of the design without the attached PLLs/clocks.
  want [ simulator
       , vsimulate
       , yoverilog2
       , fpgabin
       , timings
       ]

  ------------------------------------------------------------------------------
  -- RULE: Build simulator executable
  simulator %> \out -> do
    -- First, find all the Haskell modules, and sort them appropriately, so we
    -- can feed them to the compiler. We have to make sure the needed main
    -- module comes first.
    hs' <- getDirectoryFiles "" ["src/aap//*.hs"]
    let hs = [ "src/aap/Main.hs"
             ] ++ filter (not . ("Main.hs" `isSuffixOf`)) hs'

    clash (ClashOpts
           { clashMainIs = Nothing
           , clashObjdir = bld "obj-sim"
           , clashMode   = Simulation out
           , clashIncdir = ["src/aap"]
           })
      hs

  ------------------------------------------------------------------------------
  -- RULE: Extra Verilog code, needed so CLaSH can instantiate the iCE40 PLL.
  ice40pllv %> \out -> genVerilogForICE40 out

  ------------------------------------------------------------------------------
  -- RULE: Perform high-level synthesis with CLaSH
  clashOut %> \out -> do
    -- First, find all the Haskell modules, and 'need' them as dependencies.
    hs' <- getDirectoryFiles "" ["src/aap/AAP//*.hs"]
    need hs'

    -- next, perform high level synthesis using the clash compiler
    clash (ClashOpts
           { clashMainIs = Nothing
           , clashObjdir = bld "obj-verilog"
           , clashMode   = Verilog (bld "")
           , clashIncdir = ["src/aap"]
           })
      ["src/aap/AAP/Export/ICE40.hs"]

    -- Grab all the Verilog source files generated by Clash, and write them
    -- to an output file so we can later read their contents.
    vs <- getDirectoryFiles "" [ bld "verilog/AAP//*.v" ]
    writeFileLines out vs

  ------------------------------------------------------------------------------
  -- RULE: Generate Verilog testbench with iverilog
  vsimulate %> \out -> do
    -- Get all the Verilog source files -- but don't include the top-level
    -- module that was generated by CLaSH, as it has the reset wire hooked
    -- up to a PLL (which can't be simulated by iverilog)
    vs' <- readFileLines clashOut
    let vs = [ v | v <- vs', not ("claap.v" `isInfixOf` v) ]

    -- Generate the iverilog test bench
    iverilog out vs

  ------------------------------------------------------------------------------
  -- RULE: Perform synthesis with Yosys, yielding a compiled Verilog file
  -- without any other dependencies, and no attached PLLs.
  yoverilog2 %> \out -> do
    -- Grab all the Verilog source files, except the test bench, and the top
    -- level 'claap' module.
    vs' <- readFileLines clashOut
    let vs = [ v | v <- vs'
                 , not ("test" `isInfixOf` v)     -- no test bench
                 , not ("claap.v" `isSuffixOf` v) -- no no attached clock
                 ]

    -- Now, set-up and run Yosys
    yosys (yosysConfig
           { yosysTopModule    = "AAP_topEntity"
           , yosysSynth        = Prep
           , yosysLog          = Nothing
           , yosysSynthVerilog = Just out
           , yosysOutput       = NoNetlist
           })
      vs
    
  ------------------------------------------------------------------------------
  -- RULE: Perform verilog synthesis with Yosys
  [ yolog, yoverilog, netlist ] &%> \[ olog, overilog, onetlist ] -> do
    -- Grab all the Verilog source files, except the test bench.  Also, require
    -- the iCE40 PLL Verilog module be created in the build directory, too.
    vs' <- readFileLines clashOut
    let vs = [ v | v <- vs', not ("test" `isInfixOf` v) ]
          ++ [ ice40pllv ]

    -- Now, set-up and run Yosys
    yosys (yosysConfig
           { yosysTopModule    = "claap"
           , yosysSynth        = ICE40
           , yosysLog          = Just olog
           , yosysSynthVerilog = Just overilog
           , yosysOutput       = BLIF onetlist
           })
      vs

  ------------------------------------------------------------------------------
  -- RULE: Perform place-and-route with arachne-pnr
  [ pnrout, pnrlog ] &%> \[ out, olog ] -> do
    pcfName <- getICE40PCF
    let pcfPath = joinPath [ "src", "fpga", "ice40", pcfName ]
    arachne_pnr pcfPath olog out netlist

  ------------------------------------------------------------------------------
  -- RULE: Use IcePack to create an FPGA-ready bitstream file
  fpgabin %> \out -> icepack out pnrout

  ------------------------------------------------------------------------------
  -- RULE: Perform timing analysis report for the design
  timings %> \out -> void $ icetime True Nothing (Just out) pnrout

  ------------------------------------------------------------------------------
  -- RULE: Upload the generated image onto the FPGA
  phony "upload" $ iceprog fpgabin

  ------------------------------------------------------------------------------
  -- RULE: Run all the tests
  phony "test" $ do
    putNormal "Running all tests..."
    need [ "test-clashsim"
         , "test-iverilog"
         , "test-verilator"
         , "test-timing"
         ]

  let testCmd = cmd [ WithStdout True
                    , EchoStdout False
                    , EchoStderr False
                    , Traced ""
                    ]

  -------------------------
  -- CLaSH simulation tests
  phony "test-clashsim" $ do
    need [ simulator ]
    () <- cmdWrap simulator $ testCmd simulator
    putNormal $ "SIMULATION (CLaSH):\tOK"

  -----------------------
  -- Icarus Verilog tests
  phony "test-iverilog" $ do
    need [ vsimulate ]
    () <- cmdWrap vsimulate $ testCmd vsimulate
    putNormal $ "SIMULATION (iverilog):\tOK"

  ------------------
  -- Verilator tests
  phony "test-verilator" $ do
    --need [ vsimulate2 ]
    --() <- cmdWrap vsimulate2 $ testCmd vsimulate2
    putNormal $ "SIMULATION (verilator):\tOK"

  ----------------------------------
  -- IceTime timing analysis results
  phony "test-timing" $ do
    checkMhz <- getCheckMhz
    -- IceTime validation report
    (pathDelay, topMhz) <- icetime False checkMhz Nothing pnrout
    let info = unwords
          [ maybe "OK;" (\mhz -> "OK @ " ++ show mhz ++ "Mhz;") checkMhz
          , "path delay", "~" ++ show pathDelay ++ "ns,"
          , "max freq", "~" ++ show topMhz ++ "Mhz"
          ]
    putNormal $ "TIMING CHECK (icetime):\t" ++ info

  ------------------------------------------------------------------------------
  -- RULE: Clean up all the build artifacts
  phony "clean" $ do
    putNormal "Cleaning files in build"
    removeFilesAfter "build" ["//*"]
