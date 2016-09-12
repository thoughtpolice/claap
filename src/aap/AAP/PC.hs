-- |
-- Module      : AAP.PC
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- TODO FIXME
--
module AAP.PC
  ( ProgramCounter(..)
  , ProgramStatus(..)
  , PCRegister(..)

  , toPC, fromPC

  , pcCounter
  , pcStatus
  ) where
import CLaSH.Prelude
import Data.Bifunctor

--------------------------------------------------------------------------------

-- | 24-bit wide AAP program counter. This represents the lower 24-bits of the
-- @pc@ register, which also contains an 8-bit status code.
newtype ProgramCounter = PC (BitVector 24)

-- | 8-bit wide AAP status value. This represents the upper 8 bits of the @pc@
-- register.
newtype ProgramStatus = PS (BitVector 8)

-- | 32-bit wide AAP program counter register. This is a combination of an 8-bit
-- status register and a 24-bit counter offset.
newtype PCRegister = PR (BitVector 32)

-- | Construct a program counter register from a given counter, and a status
-- register.
toPC :: ProgramCounter -> ProgramStatus -> PCRegister
toPC (PC pc) (PS ps) = PR (ps ++# pc)

-- | Split the given register into a program counter, and the current status.
fromPC :: PCRegister -> (ProgramCounter, ProgramStatus)
fromPC (PR reg) = bimap PC PS (split reg)

pcCounter :: PCRegister -> ProgramCounter
pcCounter = fst . fromPC

pcStatus :: PCRegister -> ProgramStatus
pcStatus  = snd . fromPC

