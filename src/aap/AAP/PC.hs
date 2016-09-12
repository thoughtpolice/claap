-- |
-- Module      : AAP.PC
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Program counter for the AAP. Technically, this would normally be part of the
-- register file, but the PC register is 32-bits wide in the AAP, so for
-- simplicity and clarity we keep it separate.
--
module AAP.PC
  ( StatusReg, IpReg
  , PC

  , splitPC   -- :: PC -> (StatusReg, IpReg)
  , joinPC    -- :: (StatusReg, IpReg) -> PC

  , statusReg -- :: PC -> StatusReg
  , ipReg     -- :: PC -> IpReg

  , defaultPC -- :: PC
  ) where
import CLaSH.Prelude

--------------------------------------------------------------------------------
-- Program counter

-- | IP register: the 24-bit instruction pointer, which points inside the
-- instruction memory of the design.
type IpReg     = BitVector 24

-- | Status register: 8-bit status register.
type StatusReg = BitVector 8

-- | PC register: 32-bits wide, composed of both @'StatusReg'@ followed by
-- @'IpReg'@.
type PC        = BitVector 32

-- | Split the program counter register into the status register, and the
-- instruction pointer.
splitPC :: PC -> (StatusReg, IpReg)
splitPC = split

-- | Join the status register and IP register into the full 32-bit program
-- counter.
joinPC :: (StatusReg, IpReg) -> PC
joinPC = uncurry (++#)

-- | Get the status register out of the @'PC'@ register.
statusReg :: PC -> StatusReg
statusReg = fst . splitPC

-- | Get the instruction pointer out of the @'PC'@ register.
ipReg :: PC -> IpReg
ipReg = snd . splitPC

-- | Default program counter value.
defaultPC :: PC
defaultPC = 0x0
