-- |
-- Module      : Main
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Entry point for AAP testbench simulation.
--
module Main
  ( main -- :: IO ()
  ) where
import CLaSH.Prelude

import AAP.Types ()
import AAP.Decoder (decode)
import AAP.PC ()

import AAP.Sim.Disassembler (prettyInstr)

--------------------------------------------------------------------------------
-- Simulation driver: run the resulting /Haskell/ program.

-- | Main simulation driver.
main :: IO ()
main = do
  let f x   = pack (x :: Unsigned 32)
  let instr = f
  let prty  = prettyInstr . decode . f

  putStrLn $ prty 0x00408205 -- add   $r8,  $r0,  $r5
  putStrLn $ prty 0x1000c638 -- bne   -512, $r7,  $r0
