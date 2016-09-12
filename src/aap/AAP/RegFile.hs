{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : AAP.RegFile
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AAP register file logic. The AAP standard states that a conformant
-- implementation may have a configurable number of registers within the range
-- of @[4, 64]@ (inclusive).
--
-- Currently implemented as a simple block of data memory, taking up significant
-- logic cells.
--
module AAP.RegFile
  ( RegAddr, RegReadAddr   -- :: *
  , RegValue, RegWriteAddr -- :: *
  , RegFileWrite           -- :: *
  , regFile                -- :: ...
  ) where
import CLaSH.Prelude

import AAP.Types

--------------------------------------------------------------------------------

-- | A set of static constraints, checked by the type system, used to ensure the
-- register file is of an appropriate size, with appropriate
-- parameters. Private.
type RegFileConstraint =
  -- Ensure the proper number of registers are instantiated.
  ( 4 <= RegFileSize
  , RegFileSize <= 64
  -- Ensure the RegAddr has the proper width.
  , BitSize RegAddr ~ CLog 2 RegFileSize
  )

--------------------------------------------------------------------------------

-- | A value representing a write to the register file. A value of @'Nothing'@
-- signifies no write should occur (AKA write enablement bit = @'False'@). A
-- value of @'Just' (a, x)@ signifies a write of value @x@ to address @a@.
type RegFileWrite = Maybe (RegWriteAddr, RegValue)

-- | Register file for the AAP. Has two read ports, and a single write port. The
-- write value is represented in a fairly high level manner.
regFile :: Signal (RegReadAddr, RegReadAddr) -- ^ Read addresses
        -> Signal RegFileWrite               -- ^ Register file write
        -> Signal (RegValue, RegValue)       -- ^ Values that were read
regFile readAddrs write = result
  where
    -- the register file starts off zeroed. this uses the @'RegFileConstraint'@
    -- constraint to ensure that the type checker has our back, and will make
    -- sure the register file is of an appropriate size, with appropriate sized
    -- register addresses.
    initMem = replicate (SNat :: RegFileConstraint => SNat RegFileSize) 0

    -- unbundle the given read addresses
    (raddr1, raddr2) = unbundle readAddrs

    -- mirror the write port across both block rams, and read back the given
    -- values for the previous cycle
    rval1 = blockRam initMem raddr1 write
    rval2 = blockRam initMem raddr2 write

    -- bundle the results
    result = bundle (rval1, rval2)
