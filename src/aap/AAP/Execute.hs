{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : AAP.Execute
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Primary CPU execution engine.
--
module AAP.Execute
  ( topEntity
  ) where
import CLaSH.Prelude

import Control.Lens

import AAP.PC
import AAP.Types

import AAP.Decoder
import AAP.DMem
import AAP.IMem
import AAP.Fetch
import AAP.RegFile
import AAP.State

--------------------------------------------------------------------------------

-- | The full AAP system, tied together. Must be equipped with a set of
-- instructions to execute, which can then be synthesized to an HDL.
system :: KnownNat n
       => InstrMem n         -- ^ Instruction memory, with statically known size.
       -> Signal (Instr, PC) -- ^ Output system signal.
system instrMem = bundle (instr, cpuPcReg) where
  -- Create the instruction fetch unit out of the
  -- given set of instructions.
  fetchUnit = fetchInstr instrMem

  -- Signal for the current instruction to be executed, taken by asking the
  -- fetch unit to grab the current program counter. Note that this is mutually
  -- recursive with the definition of 'cpuPC', see below.
  instr = fetchUnit cpuPcReg

  -- Get the data memory
  _dataMem = dataMemory d256

  ( _regval1, _regval2 )
    = unbundle $ regFile (bundle (raddr1, raddr2))
                         (pure Nothing)

  -- Execute the CPU, represented as a mealy machine. Note that we delay the
  -- machine by one clock cycle using 'register' so that the first instruction
  -- can be fetched, otherwise the mutual recursion between 'instr' and 'cpuPC'
  -- would cause an infinite loop.
  result = register defaults       -- delay one cycle to get the first instr
         $ mealy (cpuMealyT cpuT)  -- mealy machine of CPU
                 defaultCpuState   -- initial CPU state
                 instr             -- instruction to execute
    where defaults = (defaultPC, 0, 0)

  -- Now, split up the wires from the output of the mealy machine transfer
  -- on this clock cyle into multiple values.
  ( cpuPcReg, raddr1, raddr2 )
    = unbundle result

topEntity :: Signal (Instr, PC)
topEntity = system (instrRomFile d7 "test.bin")

--------------------------------------------------------------------------------

-- | CPU Arithmetic Logic Unit, or \"ALU\".
alu :: AluInstr -> CpuM ()
alu instr = case instr of
  -- Pure register file ops
  NOP _ _      -> do
    readPorts .= noRead
    writePort .= noWrite
  ADD rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a + b))
  SUB rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a - b))
  AND rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a .&. b))
  OR rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a .|. b))
  XOR rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a `xor` b))
  LSL rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a `shiftL` fromIntegral b))
  LSR rd ra rb -> do
    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd (a `shiftR` fromIntegral b))
  MOV rd ra    -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd a)

  -- Register/immediate ops
  ADDI rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a + imm i))
  SUBI rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a - imm i))
  ANDI rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a .&. imm i))
  ORI  rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a .|. imm i))
  XORI rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a `xor` imm i))
  LSLI rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a `shiftL` fromIntegral i))
  LSRI rd ra i -> do
    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd (a `shiftR` fromIntegral i))
  MOVI rd i    -> do
    readPorts .= noRead
    writePort .= (\_ _ -> wr rd (imm i))

  -- Arithmetic shift (split up; slightly more verbose)
  ASR rd ra rb -> do
    -- grab the carry bit, and then clear it
    carry <- use cpuCarry <* (cpuCarry .= 0)
    -- do the computation
    let constant = extend carry `shiftL` 16

    readPorts .= (reg ra, reg rb)
    writePort .= (\a b -> wr rd $ (a .|. constant) `shiftR` (fromIntegral b))

  ASRI rd ra i -> do
    -- grab the carry bit, and then clear it
    carry <- use cpuCarry <* (cpuCarry .= 0)
    -- do the computation
    let constant = extend carry `shiftL` 16

    readPorts .= (reg ra, noReg)
    writePort .= (\a _ -> wr rd $ (a .|. constant) `shiftR` (fromIntegral i))

  -- Add/subtract with carry (split up; much more verbose)
--TODO FIXME: not invented here
--ADDC rd ra rb -> mk ra rb (\a b -> _)
--SUBC rd ra rb -> mk ra rb (\a b -> _)
  where
    noReg   = Nothing
    noRead  = (noReg, noReg)
    noWrite = \_ _ -> Nothing

    readPorts = cpuRegFile . cpuRegRead
    writePort = cpuRegFile . cpuRegWrite

    imm    = extend . pack . immUnsigned
    reg    = Just . regUnsigned
    wr d x = Just (regUnsigned d, x)

--------------------------------------------------------------------------------

-- | The primary CPU logic for the AAP, implemented as a @'State'@ @'Monad'@
-- wrapped around a @'CpuState'@. This function does not return anything; it
-- solely manipulates the @'CpuState'@ and nothing else.
--
-- This function is run with its continuous state at every clock
-- cycle. @'finishCpuT'@ is then invoked afterwords (at every cycle), which
-- bundles up the resulting values from the @'CpuState'@ into output signals,
-- used in the mealy machine tying the machine together. For example, every
-- clock cycle, the new PC for the next cycle is output from this design. This
-- is the @'cpuPC'@ component of @'CpuState'@. @'mainCpuT'@ runs the logic to
-- transform things like @'cpuPC'@ (incrementing it, for example), and
-- @'finishCpuT'@ will read those updated values, and return to the mealy
-- machine.
--
-- This is kept separate from @'finishCpuT'@ to make the logic shorter and more
-- clear.
mainCpuT :: CpuM ()
mainCpuT = do
  -- Look at the current instruction, and dispatch accordingly.
  view cpuInInstr >>= \case
    AluInstr i    -> modifying cpuPC (+1) >> alu i
    StoreInstr _  -> modifying cpuPC (+1)
    BranchInstr _ -> modifying cpuPC (+1)

    MiscInstr   _ -> do
      -- TODO FIXME: handle RTE
      modifying cpuPC (+1)
    Invalid -> return ()

-- | Utility to bundle up all the results of the CPU transformer, and return the
-- expected output values for a given clock signal. This is kept out of line
-- from @'mainCpuT'@ to make it easier to read.
finishCpuT :: CpuM (PC, RegReadAddr, RegReadAddr)
finishCpuT = (,,) <$> use cpuPC
                  <*> use (cpuRegFile . cpuRegRead . _1 . non 0)
                  <*> use (cpuRegFile . cpuRegRead . _2 . non 0)

-- | Simple alias that combines @'mainCpuT'@ and @'finishCpuT'@
-- together. Equivalent to @'mainCpuT' >> 'finishCpuT'@.
cpuT :: CpuM (PC, RegReadAddr, RegReadAddr)
cpuT = mainCpuT >> finishCpuT
