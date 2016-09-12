module Tools.Extra
  ( genVerilogForICE40
  ) where

import Development.Shake

import Config

genVerilogForICE40 :: FilePath
                   -> Action ()
genVerilogForICE40 out = do
  clash <- getClash
  Stdout vlog <- cmd [clash] "-e CLaSH.Lattice.ICE40.printPllVerilog"
  writeFileChanged out vlog
