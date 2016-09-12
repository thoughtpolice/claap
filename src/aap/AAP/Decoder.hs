{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE BinaryLiterals  #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : AAP.Decoder
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AAP instruction decoder.
--
module AAP.Decoder
  ( -- * Types: First stage decoder
    RawPiece(..), RawInstr(..)
  , RawDecodeResult(..)
    -- * Types: second stage decoder
  , MemOpClass(..)
  , Instr(..)
  , AluInstr(..)
  , StoreInstr(..)
  , BranchInstr(..)
  , MiscInstr(..)

    -- * Functions: first stage decoder
  , decodePiece, decodeRaw
  , decodeRaw16
  , decodeRaw32
  , isInstr32Bit

    -- * Functions: second stage decoder
  , decode16
  , decode32
  ) where
import CLaSH.Prelude

import AAP.Types

--------------------------------------------------------------------------------
-- Instruction decoder: part 1

-- | Raw piece of an instruction, emitted early on by the instruction decoder,
-- for every 16 bits of input in the instruction stream. By \"raw\", we mean
-- that this is before the decoder has later classified the opcode by its class
-- and arguments. In fact, this may only be one part of a multi-@'RawPiece'@
-- instruction.
--
-- The AAP architecture uses \"instruction chaining\" to extend 16 bit
-- instructions into 32 bit instructions (and could be used to extend the
-- instruction size to be as large as desired), in the same manner RISC-V
-- does. A single instruction is made out of at-least-one @'RawPiece'@
-- instruction. A 16-bit instruction has one @'RawPiece'@, while a 32-bit
-- instruction would have 2 @'RawPiece'@s, and so on.
data RawPiece
  = RawPiece
    { rawPieceOpClass  :: BitVector 2
    , rawPiece1        :: BitVector 4
    , rawPiece2        :: BitVector 3
    , rawPiece3        :: BitVector 3
    , rawPiece4        :: BitVector 3
    }
  deriving (Eq, Show)

-- | Decode a 16-bit value into a @'RawPiece' of an AAP instruction.
decodePiece :: BitVector 16 -> RawPiece
decodePiece inp = RawPiece opclass p1 p2 p3 p4
  -- 16 bit: [ xccooood, ddaaabbb ] -- little endian --> [ ddaaabbb, xccooood ]
  where
    opclass = slice d6  d5  inp
    p1      = slice d4  d1  inp
    p2      = (inp ! (0 :: Int)) ++# slice d15 d14 inp
    p3      = slice d13 d11 inp
    p4      = slice d10 d8  inp

type IsImm   = Bool

-- | A raw instruction, composed only of @'RawPiece'@s and any extra
-- metadata. Note that a @'RawInstr'@ is not necessarily a valid AAP instruction
-- in any way; it is merely a break down of the expected form a valid
-- instruction /will/ take. The second stage decoder - that consumes
-- @'RawInstr'@s, will determine if the encoding is valid.
data RawInstr
  -- | A 16-bit raw instruction, composed of only one @'RawPiece'@
  = RawInstr16
    { rawInstr16p1 :: RawPiece
    }

  -- | A 32-bit raw instruction, composed of two @'RawPiece'@s and some extra
  -- metadata.
  | RawInstr32
    { rawInstr32p1      :: RawPiece
    , rawInstr32p2      :: RawPiece
    , rawInstr32isImm   :: IsImm
      -- ^ Determines if the argument to the 32-bit instruction is an immediate,
      -- needed for some of the bitwise instructions.
    }
  deriving (Eq, Show)

data RawDecodeResult = More RawPiece | Done RawPiece
  deriving (Eq, Show)

-- | Determine if a given @'BitVector' 16@ is the bottom half of a 32-bit
-- instruction, and another 16 bit word needs to be fetched.
isInstr32Bit :: BitVector 16 -> Bool
isInstr32Bit x = high == x ! (7 :: Int)

-- | Decode a 16-bit value into a @'RawInstr'@. If the instruction should
-- actually be 32 bits, then a result is returned accordingly.
decodeRaw :: BitVector 16 -> RawDecodeResult
decodeRaw inp
  | isInstr32Bit inp = More val
  | otherwise        = Done val
  where
    val = decodePiece inp

decodeRaw16 :: BitVector 16 -> Maybe RawInstr
decodeRaw16 x = case decodeRaw x of
  More _ -> Nothing
  Done r -> Just (RawInstr16 r)

decodeRaw32 :: BitVector 32 -> Maybe RawInstr
decodeRaw32 inp = case decodeRaw bot of
  Done _ -> Nothing
  More b -> do
      (RawInstr16 t) <- decodeRaw16 top
      return (RawInstr32 t b isImm)
  where
    (bot, top) = split inp :: (BitVector 16, BitVector 16)
    isImm      = top ! (1 :: Int) == high

--------------------------------------------------------------------------------
-- Instruction decoder: part 2

-- | \"Memory operation class\" - used only for load and store instructions, to
-- determine what extra actions need to be performed upon byte/word loads or
-- stores.
data MemOpClass
  = PostInc -- ^ Increment the address after the load/store.
  | PreDec  -- ^ Decrement the address before the load/store.
  | None    -- ^ Do nothing after the load/store
  deriving (Eq, Show)

-- | An AAP instruction, in either 16 or 32 bit form, after decoding. This is
-- the output of the second-stage of the decoder, which is given @'RawInstr'@s
-- and outputs ordinary @'Instr'@s. Note that 16 bit instructions have their
-- operands extended to fit in this form; the size of each operand is the native
-- size of the 32 bit encoding.
--
-- Alternatively, if an instruction could not be properly decoded, an
-- @'Invalid'@ value is returned.
data Instr
  = AluInstr    AluInstr
  | StoreInstr  StoreInstr
  | BranchInstr BranchInstr
  | MiscInstr   MiscInstr
  | Invalid
  deriving (Eq, Show)

data AluInstr
  = NOP  (Reg 'D 6) (Imm 'I 12)
  | ADD  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | SUB  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | AND  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | OR   (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | XOR  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | ASR  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | LSL  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | LSR  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | MOV  (Reg 'D 6) (Reg 'A 6)

  | ADDI (Reg 'D 6) (Reg 'A 6) (Imm 'I 10)
  | SUBI (Reg 'D 6) (Reg 'A 6) (Imm 'I 10)
  | ANDI (Reg 'D 6) (Reg 'A 6) (Imm 'I 9)
  | ORI  (Reg 'D 6) (Reg 'A 6) (Imm 'I 9)
  | XORI (Reg 'D 6) (Reg 'A 6) (Imm 'I 9)
  | ASRI (Reg 'D 6) (Reg 'A 6) (Imm 'I 6)
  | LSLI (Reg 'D 6) (Reg 'A 6) (Imm 'I 6)
  | LSRI (Reg 'D 6) (Reg 'A 6) (Imm 'I 6)
  | MOVI (Reg 'D 6) (Imm 'I 16)

  | ADDC (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | SUBC (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  deriving (Eq, Show)

data StoreInstr
  = LDB MemOpClass (Reg 'D 6) (Reg 'A 6)  (Imm 'S 10)
  | LDW MemOpClass (Reg 'D 6) (Reg 'A 6)  (Imm 'S 10)
  | STB MemOpClass (Reg 'D 6) (Imm 'S 10) (Reg 'A 6)
  | STW MemOpClass (Reg 'D 6) (Imm 'S 10) (Reg 'A 6)
  deriving (Eq, Show)

data BranchInstr
  = BRA  (Imm 'S 22)
  | BAL  (Imm 'S 16) (Reg 'B 6)
  | BEQ  (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BNE  (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLTS (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLES (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLTU (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLEU (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)

  | JMP  (Reg 'D 6)
  | JAL  (Reg 'D 6) (Reg 'B 6)
  | JEQ  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JNE  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTS (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLES (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTU (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLEU (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)

  | JMPL  (Reg 'D 6)
  | JALL  (Reg 'D 6) (Reg 'B 6)
  | JEQL  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JNEL  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTSL (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLESL (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTUL (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLEUL (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  deriving (Eq, Show)

data MiscInstr
  = RTE (Reg 'D 3)
  deriving (Eq, Show)

-- | Second-stage of the instruction decoder: decodes a @'RawInstr'@ into the
-- corresponding 16 or 32 bit instruction.
decodeInstr :: RawInstr -> Instr

-- The first case, 16-bit instructions, are relatively easy, but still tedious.
decodeInstr (RawInstr16 RawPiece{..}) = result where
  -- for the 16 bit case, we have to extend the operands.
  r = toReg  . zeroExtend
  i = toImm  . zeroExtend
  -- wew, what a dance for signed immediates
  s = toSImm
    . pack
    . signExtend
    . (unpack :: KnownNat n => BitVector n -> Signed n)

  result = case rawPieceOpClass of
    -- ALU instructions
    0b00 -> case rawPiece1 of
      0b0000 -> AluInstr $ NOP  (r rawPiece2) (i (rawPiece3 ++# rawPiece4))
      0b0001 -> AluInstr $ ADD  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0010 -> AluInstr $ SUB  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0011 -> AluInstr $ AND  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0100 -> AluInstr $ OR   (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0101 -> AluInstr $ XOR  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0110 -> AluInstr $ ASR  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0111 -> AluInstr $ LSL  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1000 -> AluInstr $ LSR  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1001 -> AluInstr $ MOV  (r rawPiece2) (r rawPiece3)
      0b1010 -> AluInstr $ ADDI (r rawPiece2) (r rawPiece3) (i rawPiece4)
      0b1011 -> AluInstr $ SUBI (r rawPiece2) (r rawPiece3) (i rawPiece4)
      0b1100 -> AluInstr $ ASRI (r rawPiece2) (r rawPiece3) (i rawPiece4)
      0b1101 -> AluInstr $ LSLI (r rawPiece2) (r rawPiece3) (i rawPiece4)
      0b1110 -> AluInstr $ LSRI (r rawPiece2) (r rawPiece3) (i rawPiece4)
      0b1111 -> AluInstr $ MOVI (r rawPiece2) (i (rawPiece3 ++# rawPiece4))
      _      -> Invalid

    -- Load/store instructions
    0b01 -> case rawPiece1 of
      0b0000 -> StoreInstr $ LDB None    (r rawPiece2) (r rawPiece3) (s rawPiece4)
      0b0100 -> StoreInstr $ LDW None    (r rawPiece2) (r rawPiece3) (s rawPiece4)
      0b0001 -> StoreInstr $ LDB PostInc (r rawPiece2) (r rawPiece3) (s rawPiece4)
      0b0101 -> StoreInstr $ LDW PostInc (r rawPiece2) (r rawPiece3) (s rawPiece4)
      0b0010 -> StoreInstr $ LDB PreDec  (r rawPiece2) (r rawPiece3) (s rawPiece4)
      0b0110 -> StoreInstr $ LDW PreDec  (r rawPiece2) (r rawPiece3) (s rawPiece4)
      0b1000 -> StoreInstr $ STB None    (r rawPiece2) (s rawPiece4) (r rawPiece3)
      0b1100 -> StoreInstr $ STW None    (r rawPiece2) (s rawPiece4) (r rawPiece3)
      0b1001 -> StoreInstr $ STB PostInc (r rawPiece2) (s rawPiece4) (r rawPiece3)
      0b1101 -> StoreInstr $ STW PostInc (r rawPiece2) (s rawPiece4) (r rawPiece3)
      0b1010 -> StoreInstr $ STB PreDec  (r rawPiece2) (s rawPiece4) (r rawPiece3)
      0b1110 -> StoreInstr $ STW PreDec  (r rawPiece2) (s rawPiece4) (r rawPiece3)
      _      -> Invalid

    -- Branch instructions
    0b10 -> case rawPiece1 of
      0b0000 -> BranchInstr $ BRA  (s (rawPiece2 ++# rawPiece3 ++# rawPiece4))
      0b0001 -> BranchInstr $ BAL  (s (rawPiece2 ++# rawPiece3)) (r rawPiece4)
      0b0010 -> BranchInstr $ BEQ  (s rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0011 -> BranchInstr $ BNE  (s rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0100 -> BranchInstr $ BLTS (s rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0101 -> BranchInstr $ BLES (s rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0110 -> BranchInstr $ BLTU (s rawPiece2) (r rawPiece3) (r rawPiece4)
      0b0111 -> BranchInstr $ BLEU (s rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1000 -> BranchInstr $ JMP  (r rawPiece2)
      0b1001 -> BranchInstr $ JAL  (r rawPiece2) (r rawPiece4)
      0b1010 -> BranchInstr $ JEQ  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1011 -> BranchInstr $ JNE  (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1100 -> BranchInstr $ JLTS (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1101 -> BranchInstr $ JLES (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1110 -> BranchInstr $ JLTU (r rawPiece2) (r rawPiece3) (r rawPiece4)
      0b1111 -> BranchInstr $ JLEU (r rawPiece2) (r rawPiece3) (r rawPiece4)
      _      -> Invalid

    -- Miscellaneous instructions
    0b11 -> case rawPiece1 of
      0b0000 -> MiscInstr $ RTE (r rawPiece2)
      _      -> Invalid

    -- Done
    _ -> Invalid

-- The 32-bit case is even more tedious to handle, and requires merging
-- values from both p1 and p2.
decodeInstr (RawInstr32 p1 p2 isImm) = result where
  r = toReg
  i = toImm
  s = toSImm

  result = case rawPieceOpClass p2 of
    -- ALU instructions
    0b00 -> case rawPiece1 p2 of
      0b0000 -> AluInstr $ NOP (r (rawPiece2 p1 ++# rawPiece2 p2))
                               (i (rawPiece3 p1 ++# rawPiece4 p1
                               ++# rawPiece3 p2 ++# rawPiece4 p2))

      0b0001 -> AluInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then ADDC else ADD

      0b0010 -> AluInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then SUBC else SUB

      0b0011 -> AluInstr $ if not isImm
        then AND  (r (rawPiece2 p1 ++# rawPiece2 p2))
                  (r (rawPiece3 p1 ++# rawPiece3 p2))
                  (r (rawPiece4 p1 ++# rawPiece4 p2))
        else ANDI (r (rawPiece2 p1 ++# rawPiece2 p2))
                  (r (rawPiece3 p1 ++# rawPiece3 p2))
                  (i (slice d3 d1 (rawPiece1 p1) ++# rawPiece4 p1 ++# rawPiece4 p2))
      0b0100 -> AluInstr $ if not isImm
        then OR  (r (rawPiece2 p1 ++# rawPiece2 p2))
                 (r (rawPiece3 p1 ++# rawPiece3 p2))
                 (r (rawPiece4 p1 ++# rawPiece4 p2))
        else ORI (r (rawPiece2 p1 ++# rawPiece2 p2))
                 (r (rawPiece3 p1 ++# rawPiece3 p2))
                 (i (slice d3 d1 (rawPiece1 p1) ++# rawPiece4 p1 ++# rawPiece4 p2))
      0b0101 -> AluInstr $ if not isImm
        then XOR  (r (rawPiece2 p1 ++# rawPiece2 p2))
                  (r (rawPiece3 p1 ++# rawPiece3 p2))
                  (r (rawPiece4 p1 ++# rawPiece4 p2))
        else XORI (r (rawPiece2 p1 ++# rawPiece2 p2))
                  (r (rawPiece3 p1 ++# rawPiece3 p2))
                  (i (slice d3 d1 (rawPiece1 p1) ++# rawPiece4 p1 ++# rawPiece4 p2))
      0b0110 -> AluInstr $ ASR (r (rawPiece2 p1 ++# rawPiece2 p2))
                               (r (rawPiece3 p1 ++# rawPiece3 p2))
                               (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0111 -> AluInstr $ LSL (r (rawPiece2 p1 ++# rawPiece2 p2))
                               (r (rawPiece3 p1 ++# rawPiece3 p2))
                               (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b1000 -> AluInstr $ LSR (r (rawPiece2 p1 ++# rawPiece2 p2))
                               (r (rawPiece3 p1 ++# rawPiece3 p2))
                               (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b1001 -> AluInstr $ MOV (r (rawPiece2 p1 ++# rawPiece2 p2))
                               (r (rawPiece3 p1 ++# rawPiece3 p2))
      0b1010 -> AluInstr $ ADDI (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (i (rawPiece1 p1 ++# rawPiece4 p1 ++# rawPiece4 p2))
      0b1011 -> AluInstr $ SUBI (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (i (rawPiece1 p1 ++# rawPiece4 p1 ++# rawPiece4 p2))
      0b1100 -> AluInstr $ ASRI (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (i (rawPiece4 p1 ++# rawPiece4 p2))
      0b1101 -> AluInstr $ LSLI (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (i (rawPiece4 p1 ++# rawPiece4 p2))
      0b1110 -> AluInstr $ LSRI (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (r (rawPiece3 p1 ++# rawPiece3 p2))
                                (i (rawPiece4 p1 ++# rawPiece4 p2))
      0b1111 -> AluInstr $ MOVI (r (rawPiece2 p1 ++# rawPiece2 p2))
                                (i (rawPiece1 p1 ++# rawPiece3 p1 ++# rawPiece4 p1
                                ++# rawPiece3 p2 ++# rawPiece4 p2))

      _  -> Invalid

    -- Load/store instructions
    0b01 -> case rawPiece1 p2 of
      0b0000 -> StoreInstr $ LDB None    (r ddd) (r aaa) (s sss)
      0b0100 -> StoreInstr $ LDW None    (r ddd) (r aaa) (s sss)
      0b0001 -> StoreInstr $ LDB PostInc (r ddd) (r aaa) (s sss)
      0b0101 -> StoreInstr $ LDW PostInc (r ddd) (r aaa) (s sss)
      0b0010 -> StoreInstr $ LDB PreDec  (r ddd) (r aaa) (s sss)
      0b0110 -> StoreInstr $ LDW PreDec  (r ddd) (r aaa) (s sss)
      0b1000 -> StoreInstr $ STB None    (r ddd) (s sss) (r aaa)
      0b1100 -> StoreInstr $ STW None    (r ddd) (s sss) (r aaa)
      0b1001 -> StoreInstr $ STB PostInc (r ddd) (s sss) (r aaa)
      0b1101 -> StoreInstr $ STW PostInc (r ddd) (s sss) (r aaa)
      0b1010 -> StoreInstr $ STB PreDec  (r ddd) (s sss) (r aaa)
      0b1110 -> StoreInstr $ STW PreDec  (r ddd) (s sss) (r aaa) -- ERRATA
      _      -> Invalid
      where
        ddd = rawPiece2 p1 ++# rawPiece2 p2
        aaa = rawPiece3 p1 ++# rawPiece3 p2
        sss = rawPiece1 p1 ++# rawPiece4 p1 ++# rawPiece4 p2

    -- Branch/jump instructions
    0b10 -> case rawPiece1 p2 of
      0b0000 -> BranchInstr $ BRA (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece3 p1
                                  ++# rawPiece4 p1 ++# rawPiece2 p2 ++# rawPiece3 p2
                                  ++# rawPiece4 p2))
      0b0001 -> BranchInstr $ BAL (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece3 p1 ++# rawPiece2 p2 ++# rawPiece3 p2))
                                  (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0010 -> BranchInstr $ BEQ (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2))
                                  (r (rawPiece3 p1 ++# rawPiece3 p2))
                                  (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0011 -> BranchInstr $ BNE (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2))
                                  (r (rawPiece3 p1 ++# rawPiece3 p2))
                                  (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0100 -> BranchInstr $ BLTS (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0101 -> BranchInstr $ BLES (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0110 -> BranchInstr $ BLTU (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b0111 -> BranchInstr $ BLEU (s (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
      0b1000 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
        where oper = if isImm then JMPL else JMP
      0b1001 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JALL else JAL
      0b1010 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JEQL else JEQ
      0b1011 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JNEL else JNE
      0b1100 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JLTSL else JLTS
      0b1101 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JLESL else JLES
      0b1110 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JLTUL else JLTU
      0b1111 -> BranchInstr $ oper (r (rawPiece2 p1 ++# rawPiece2 p2))
                                   (r (rawPiece3 p1 ++# rawPiece3 p2))
                                   (r (rawPiece4 p1 ++# rawPiece4 p2))
        where oper = if isImm then JLEUL else JLEU
      _ -> Invalid

    -- Done
    _ -> Invalid

--------------------------------------------------------------------------------
-- Instruction decoder: full interface

decode16 :: BitVector 16 -> Instr
decode16 = maybe Invalid decodeInstr . decodeRaw16

decode32 :: BitVector 32 -> Instr
decode32 = maybe Invalid decodeInstr . decodeRaw32
