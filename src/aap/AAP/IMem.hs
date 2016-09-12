{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : AAP.IMem
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Instruction memory, represented as a ROM -- where code is stored and where
-- the fetch unit receives instructions from.
--
-- Mostly used as a simple wrapper around the CLaSH ROM utilities - while in
-- theory the instructions could be read-or-write and not just read-only, the
-- implementation here treates them as a static, non-modifiable asynchronous
-- ROM, to be embedded into the design.
--
module AAP.IMem
  ( InstrMem     -- :: *
  , instrRom     -- :: KnownNat n => Vec n (BitVector 16) -> InstrMem
  , instrRomFile -- :: SNat s -> FilePath                 -> InstrMem
  ) where
import CLaSH.Prelude

import AAP.Types              ( InstrAddr )

-- | An interface to an \"instruction memory unit\": for our purposes,
-- instruction memory is represented as a function from @readAddress -> value@,
-- where @value@ is a 16-bit word and @readAddress@ is a 24-bit value to read
-- from. The @n@ parameter is a type which denotes how large the instruction
-- memory is (in units of a Word, i.e. 16-bit values).
type InstrMem n = (SNat n, InstrAddr -> BitVector 16)

-- | Create an instruction memory unit out of a vector of 16 bit values. When
-- synthesized, this will statically be embedded into the resulting design. You
-- can then query the resulting function to retreive instructions.
instrRom :: forall n. KnownNat n
         => Vec n (BitVector 16) -- ^ Vector describing the instructions.
         -> InstrMem n
instrRom instrs = (SNat :: SNat n, asyncRom instrs)
{-# NOINLINE instrRom #-}

-- | Create an instruction memory unit out of a binary ROM file with a known
-- size. When synthesized, this will statically be embedded into the resulting
-- design. You can then query the resulting function to retreive instructions.
-- instructions.
instrRomFile :: SNat n
             -> FilePath
             -> InstrMem n
instrRomFile sz path = (sz, asyncRomFile sz path)
{-# NOINLINE instrRomFile #-}
