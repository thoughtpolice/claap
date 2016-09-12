-- |
-- Module      : AAP.Sim.Test
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Entry point for all the simulation tool tests.
--
module AAP.Sim.Test
  ( runDisassemblerTests -- :: IO ()
  ) where
import Prelude

import AAP.Sim.Test.Disassembler as Disassembler

-- | Run all tests for the disassembler.
runDisassemblerTests :: IO ()
runDisassemblerTests = mapM_ Disassembler.checkFile
  [ "src/t/disasm/store.txt"
  , "src/t/disasm/branch.txt"
  , "src/t/disasm/alu.txt"
  , "src/t/disasm/load.txt"
  , "src/t/disasm/noop.txt"
  , "src/t/disasm/move.txt"
  ]
