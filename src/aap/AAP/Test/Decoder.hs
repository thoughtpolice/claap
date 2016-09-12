-- |
-- Module      : AAP.Test.Decoder
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tests for the 16-bit instruction decoder. Suitable for post place-and-route
-- testing, and interactive simulation testing:
--
-- @
-- >>> 'simulationOutput'
-- [@'TestOK'@,@'TestOK'@,@'TestOK'@]
-- @
--
module AAP.Test.Decoder
  ( -- * Synthesis testing.
    topEntity        -- :: Signal (BitVector 16) -> Signal Instr
  , testInput        -- :: Signal (BitVector 16)
  , expectedOutput   -- :: Signal Instr -> Signal Bool

    -- * Simulation testing.
  , simulationResults -- :: [SimulationTestResult]
  ) where
import CLaSH.Prelude
import CLaSH.Testbench.Fancy

import AAP.Decoder

--------------------------------------------------------------------------------
-- Test description

-- | Trivial circuit for testing the 16 bit instruction decoder.
topEntity :: Signal (BitVector 16) -> Signal Instr
topEntity = fmap decode16

-- | The number of tests in this test bench.
type NumTests = (4 :: Nat)

-- | Inputs into the test bench; these are run through @'topEntity'@ and checked
-- against @'output'@.
input :: Vec NumTests (BitVector 16)
input = 0b0000000000000010
     :> 0b1110001100000101
     :> 0b1111100000000110
     :> 0b0000000000000000
     :> Nil

-- | Expected outputs from running @'topEntity'@ on the test @'input'@.
output :: Vec NumTests Instr
output = vals where
  vals = AluInstr (ADD 0 0 0)
      :> AluInstr (SUB 7 4 3)
      :> AluInstr (AND 3 7 0)
      :> AluInstr (NOP 0 0)
      :> Nil

--------------------------------------------------------------------------------
-- Synthesis test harness.

-- | Input value signal for synthesis tests.
testInput :: Signal (BitVector 16)
testInput = stimuliGenerator input

-- | Output verifier for synthesis tests.
expectedOutput :: Signal Instr -> Signal Bool
expectedOutput = outputVerifier output

--------------------------------------------------------------------------------
-- Simulation test harness

-- | Results of running all the tests. __NB__: This is only meant for running
-- CLaSH simulation tests.
simulationResults :: [SimulatedTestResult]
simulationResults = simulationTest output (topEntity testInput)
