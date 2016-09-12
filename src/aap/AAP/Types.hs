-- |
-- Module      : AAP.Types
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Basic type definitions used throughout the project.
--
module AAP.Types
  ( -- * Types
    W(..)
  ) where
import CLaSH.Prelude

--------------------------------------------------------------------------------
-- Types

-- | 16-bit word value for the AAP architecture.
newtype W = W (BitVector 16)
