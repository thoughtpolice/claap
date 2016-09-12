{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  ( -- * Type-level values.
    RegFileSize

    -- * Types
  , W

    -- ** Types for the register file and memory
  , MemAddr, MemValue
  , MemReadAddr, MemWriteAddr
  , RegAddr, RegValue
  , RegReadAddr, RegWriteAddr
  , InstrAddr

  , Reg, RegKind(..), toReg, regBV, regUnsigned
  , Imm, ImmKind(..), toImm, toSImm, immSigned, immUnsigned
  , immUBV, immSBV
  ) where
import CLaSH.Prelude

--------------------------------------------------------------------------------
-- Basic types

-- | Size of the register file.
type RegFileSize = 64

-- | 16-bit word value for the AAP architecture.
type W = 16

-- | Memory address: used to specify where to read from and write to, when
-- addressing values inside the data memory unit.
type MemAddr  = BitVector 8

-- | Memory value, read out from the data memory at a specified @'MemAddr'@.
type MemValue = BitVector W

-- | Register value, read out from the register file at a specified @'RegAddr'@.
type RegValue = BitVector W

-- | Fetch address: used to specify where to read from in the instruction
-- memory. Because the program counter is 24-bits, this means there is a total
-- of 2^24 = 16MB of instruction memory that is addressable.
type InstrAddr = BitVector 24

-- | Address to write to in the register file. Constrained to 6 bits, which
-- allows a maximum address value of 63 (@0b111111@), allowing 64 registers (as
-- defined by the AAP standard).
type RegAddr  = Unsigned 6

-- | Simple alias to make some types read better. Used to distinguish read ports
-- from the write port.
type RegWriteAddr = RegAddr

-- | Simple alias to make the types read better. Used to distinguish read ports
-- from the write port.
type RegReadAddr  = RegAddr

-- | Simple alias to make some types read better. Used to distinguish read ports
-- from the write port.
type MemWriteAddr = RegAddr

-- | Simple alias to make the types read better. Used to distinguish read ports
-- from the write port.
type MemReadAddr  = RegAddr

--------------------------------------------------------------------------------
-- Decoder/instruction types

-- | The \"kind\" of a register reference in an instruction. AAP instructions
-- use a simple 3-operand code, so there are generally up-to three register
-- references: the /destination/ register, and two source registers /A/ and /B/.
--
-- Note: This is used as a /kind/, not a type - it is promoted to the kind level
-- with @DataKinds@.
data RegKind
  = D -- ^ The destination register.
  | A -- ^ Source register A.
  | B -- ^ Source register B.

-- | The \"kind\" of an immediate value in an instruction. Used to classify
-- whether the immediate is signed or unsigned.
--
-- Note: This is used as a /kind/, not a type - it is promoted to the kind level
-- with @DataKinds@.
data ImmKind
  = S -- ^ Signed immediate value
  | I -- ^ Unsigned immediate value

-- | Simple type alias, used to give mnemonics to the instruction encoding.
newtype Reg (k :: RegKind) s = Reg (BitVector s)
  deriving (Eq, Show, Integral, Enum, Real, Num, Ord)

-- | Convert a @'BitVector'@ to a @'Reg'@ value. Can be of any @'RegKind'@, as
-- long as it's the appropriate size.
toReg :: KnownNat s => BitVector s -> Reg k s
toReg = Reg

-- | Convert a @'Reg'@ to a @'BitVector'@.
regBV :: Reg k s -> BitVector s
regBV (Reg s) = s

-- | Convert a @'Reg'@ to a @'Unsigned'@ value.
regUnsigned :: Reg k s -> Unsigned s
regUnsigned = unpack . regBV

-- | Simple type alias, used to give mnemonics to the instruction encoding.
newtype Imm (k :: ImmKind) s = Imm (BitVector s)
  deriving (Eq, Show, Integral, Enum, Real, Num, Ord)

-- | Convert a @'BitVector'@ to a signed @'Imm'@ value.
toImm :: BitVector s -> Imm 'I s
toImm = Imm

-- | Convert a @'BitVector'@ to a signed @'Imm'@ value.
toSImm :: BitVector s -> Imm 'S s
toSImm = Imm

-- | Convert an @'Imm'@ to an @'Unsigned'@ Number.
immUnsigned :: KnownNat s => Imm 'I s -> Unsigned s
immUnsigned (Imm s) = unpack s

-- | Convert an @'Imm'@ to a @'Signed'@ number.
immSigned :: KnownNat s => Imm 'S s -> Signed s
immSigned (Imm s) = unpack s

immUBV :: KnownNat s => Imm 'I s -> BitVector s
immUBV = pack . immUnsigned

immSBV :: KnownNat s => Imm 'S s -> BitVector s
immSBV = pack . immSigned
