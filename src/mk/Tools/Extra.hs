module Tools.Extra
  ( genVerilogForICE40
  ) where

import Development.Shake.Fancy

import Config

genVerilogForICE40 :: FilePath
                   -> Action ()
genVerilogForICE40 out = do
  let act = "CLaSH.Lattice.ICE40.printPllVerilog"
  clash <- getClash
  Stdout vlog <- cmdWrap clash $ cmd clash ["-e", act]
  writeFileChanged out vlog
