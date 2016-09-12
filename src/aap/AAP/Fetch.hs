-- |
-- Module      : AAP.Fetch
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Instruction fetch unit, used by the execution unit.
--
module AAP.Fetch
  ( fetchInstr -- :: ...
  ) where
import CLaSH.Prelude

import AAP.PC
import AAP.Decoder
import AAP.IMem
import AAP.Types

-- | Fetch an instruction from a read address signal, returning a signal
-- containing decoded values. This is internally implemented as a mealy machine,
-- with a one-cycle delay for 32-bit instructions - the fetch unit will emit a
-- 16-bit @NOP@ instruction upon encountering the upper half of a 32 bit
-- instruction, with the full instruction in the following cycle. 16 bit
-- instructions only take one clock cycle to fetch.
--
-- The intended usage pattern is something like:
--
-- @
-- instrMem  = 'AAP.IMem.instrRomFile' 'd256' "instr.bin" -- or 'AAP.IMem.instrRom'
-- fetchUnit = fetchInstr instrMem
-- @
--
-- Afterwords, you then have @fetchUnit@ function of type @'Signal' 'InstAddr'
-- -> 'Signal' 'Instr'@. For example, if you ran the above in a REPL and wanted
-- to retreive the first instruction in some piece of memory:
--
-- >>> 'sampleN' 1 $ fetchUnit (pure 0)
--
-- To retrieve a set of instructions, as the instruction fetch unit will receive
-- them from some given instruction memory ROM:
--
-- >>> 'sampleN' 5 $ (fetchUnit (fromList [0,1,2,3,4,5]))
--
fetchInstr :: KnownNat n
           => InstrMem n       -- ^ Instruction Memory
           -> Signal PC        -- ^ Signal containing addresses to read from
           -> Signal Instr     -- ^ Resulting instruction signal.
fetchInstr (sz, imem) = mealy fetchT (False, 0)
  where
    -- fetch transformer
    fetchT :: (Bool, BitVector 16) -> PC -> ((Bool, BitVector 16), Instr)
    fetchT (isDelayed, lastWord) pc = ((newDelay, rval), out)
      where
        -- grab the 16 bit word
        addr  = ipReg pc
        rval  = imem addr
        -- see if it's 32 bit
        is32  = isInstr32Bit rval
        -- see if the *next* PC addr is out of range
        isOOB = (addr + 1) >= fromInteger (snatToInteger sz)

        -- figure out the output value and new delay value
        (newDelay, out) = case isDelayed of
          -- last cycle was delayed to read a 32 bit instruction; we now have
          -- the remaining 16 bits, so decode as a 32-bit instruction.
          True  -> (False, decode32 (lastWord ++# rval))

          -- we aren't delayed, so examine the word we just read
          False -> case is32 of

            -- it's not 32 bit, so just read a 16 bit value
            False -> (False, decode16 rval)

            -- it is 32 bit, but the next value is out of range, so fail.
            True | isOOB -> (False, Invalid)

            -- it is 32 bit, so 16-bit NOP for this cycle, try again next cycle
            True -> (True, AluInstr $ NOP (toReg 0) (toImm 1))
