-- |
-- Module      : AAP.DMem
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Data memory unit, separated from the instruction memory unit.
--
module AAP.DMem
  ( dataMemory -- :: ...
  ) where
import CLaSH.Prelude

import AAP.Types

--------------------------------------------------------------------------------

dataMemory :: ( KnownNat n          -- Memory block size must be static
              , w ~ BitSize MemAddr -- Size of the memory address operand
              , n <= 2^w            -- Block has to be big enough for addreses
              )
           => SNat n            -- ^ Size of the memory
           -> Signal MemAddr    -- ^ Read address
           -> Signal (Maybe (MemAddr, MemValue)) -- ^ Write value
           -> Signal MemValue   -- ^ Data out
dataMemory size = blockRam (replicate size 0)
