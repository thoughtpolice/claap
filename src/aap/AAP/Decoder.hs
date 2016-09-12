{-# LANGUAGE DataKinds       #-}
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
-- TODO FIXME
--
module AAP.Decoder
  ( MemOpClass(..)
  , Instr16(..)
  , Instr32(..)
  , Instr(..)
  , decode
  ) where
import CLaSH.Prelude

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
  where
    opclass = slice d14 d13 inp
    p1      = slice d12 d9  inp
    p2      = slice d8  d6  inp
    p3      = slice d5  d3  inp
    p4      = slice d2  d0  inp

type IsImm   = Bool
type IsExtra = Bool

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
    , rawInstr32isExtra :: IsExtra
      -- ^ Determines the 'extra' information in the 32-bit instruction, used in
      -- add-with-carry in the ALU and some long-jump opcodes.
    }
  deriving (Eq, Show)

-- | First-stage decoder: Decode a 32-bit value into a @'RawInstr'@. Note that
-- the given @'RawInstr'@ is /not/ checked or necessarily a valid AAP
-- instruction in any form, and the metadata might be bogus. Only the
-- second-stage decoder will be able to properly classify valid/invalid
-- instructions.
decodeRaw :: BitVector 32 -> RawInstr
decodeRaw inp
  | isHigh inp = decodeRaw32 inp
  | otherwise  = decodeRaw16 (snd $ split inp)
  where
    isHigh :: BitVector 32 -> Bool
    isHigh x = high == x ! (15 :: Int)

-- | Decode a 16-bit value into a @'RawInstr'@.
decodeRaw16 :: BitVector 16 -> RawInstr
decodeRaw16 inp = RawInstr16 top where
  top = decodePiece inp

-- | Decode a 32-bit value into a @'RawInstr'@.
decodeRaw32 :: BitVector 32 -> RawInstr
decodeRaw32 inp = RawInstr32 top bot isImm isExtra where
  top = decodePiece (slice d31 d16 inp)
  bot = decodePiece (slice d15 d0  inp)

  isImm   = inp ! (9 :: Int) == high
  isExtra = slice d15 d9 inp == 0

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
type Reg (k :: RegKind) s = BitVector s

-- | Simple type alias, used to give mnemonics to the instruction encoding.
type Imm (k :: ImmKind) s = BitVector s

-- | A 16-bit AAP instruction.
data Instr16
  -- ALU instructions
  = NOP16  (Reg 'D 3) (Imm 'I 6)
  | ADD16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | SUB16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | AND16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | OR16   (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | XOR16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | ASR16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | LSL16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | LSR16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | MOV16  (Reg 'D 3) (Reg 'A 3)
  | ADDI16 (Reg 'D 3) (Reg 'A 3) (Imm 'I 3)
  | SUBI16 (Reg 'D 3) (Reg 'A 3) (Imm 'I 3)
  | ASRI16 (Reg 'D 3) (Reg 'A 3) (Imm 'I 3)
  | LSLI16 (Reg 'D 3) (Reg 'A 3) (Imm 'I 3)
  | LSRI16 (Reg 'D 3) (Reg 'A 3) (Imm 'I 3)
  | MOVI16 (Reg 'D 3) (Imm 'I 6)

  -- Load/Store instructions
  | LDB16 MemOpClass (Reg 'D 3) (Reg 'A 3) (Imm 'S 3)
  | LDW16 MemOpClass (Reg 'D 3) (Reg 'A 3) (Imm 'S 3)
  | STB16 MemOpClass (Reg 'D 3) (Reg 'A 3) (Imm 'S 3)
  | STW16 MemOpClass (Reg 'D 3) (Reg 'A 3) (Imm 'S 3)

  -- Branch/jump instructions
  | BRA16 (Imm 'S 9)
  | BAL16 (Imm 'S 6) (Reg 'B 3)
  | BEQ16 (Imm 'S 3) (Reg 'A 3) (Reg 'B 3)
  | BNE16  (Imm 'S 3) (Reg 'A 3) (Reg 'B 3)
  | BLTS16 (Imm 'S 3) (Reg 'A 3) (Reg 'B 3)
  | BLES16 (Imm 'S 3) (Reg 'A 3) (Reg 'B 3)
  | BLTU16 (Imm 'S 3) (Reg 'A 3) (Reg 'B 3)
  | BLEU16 (Imm 'S 3) (Reg 'A 3) (Reg 'B 3)
  | JMP16  (Reg 'D 3)
  | JAL16  (Reg 'D 3) (Reg 'B 3)
  | JEQ16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | JNE16  (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | JLTS16 (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | JLES16 (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | JLTU16 (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)
  | JLEU16 (Reg 'D 3) (Reg 'A 3) (Reg 'B 3)

  -- Miscellaneous instructions
  | RTE16 (Reg 'D 3)
  deriving (Eq, Show)

-- | A 32-bit AAP instruction.
data Instr32
  -- ALU instructions
  = NOP32  (Reg 'D 6) (Imm 'I 12)
  | ADD32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | SUB32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | AND32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | OR32   (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | XOR32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | ASR32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | LSL32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | LSR32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | MOV32  (Reg 'D 6) (Reg 'A 6)
  | ADDI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 10)
  | SUBI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 10)
  | ASRI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 6)
  | LSLI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 6)
  | LSRI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 6)
  | MOVI32 (Reg 'D 6) (Imm 'I 16)

  | ADDC32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | SUBC32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | ANDI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 9)
  | ORI32  (Reg 'D 6) (Reg 'A 6) (Imm 'I 9)
  | XORI32 (Reg 'D 6) (Reg 'A 6) (Imm 'I 9)

  -- Load/Store instructions
  | LDB32 MemOpClass (Reg 'D 6) (Reg 'A 6) (Imm 'S 10)
  | LDW32 MemOpClass (Reg 'D 6) (Reg 'A 6) (Imm 'S 10)
  | STB32 MemOpClass (Reg 'D 6) (Reg 'A 6) (Imm 'S 10)
  | STW32 MemOpClass (Reg 'D 6) (Reg 'A 6) (Imm 'S 10)

  -- Branch/jump instructions
  | BRA32  (Imm 'S 22)
  | BAL32  (Imm 'S 16) (Reg 'B 6)
  | BEQ32  (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BNE32  (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLTS32 (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLES32 (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLTU32 (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | BLEU32 (Imm 'S 10) (Reg 'A 6) (Reg 'B 6)
  | JMP32  (Reg 'D 6)
  | JAL32  (Reg 'D 6) (Reg 'B 6)
  | JEQ32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JNE32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTS32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLES32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTU32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLEU32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)

  | JMPL32  (Reg 'D 6)
  | JALL32  (Reg 'D 6) (Reg 'B 6)
  | JEQL32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JENL32  (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTSL32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLESL32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLTUL32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  | JLEUL32 (Reg 'D 6) (Reg 'A 6) (Reg 'B 6)
  deriving (Eq, Show)

-- | An AAP instruction, in either 16 or 32 bit form. This is the output of the
-- second-stage of the decoder, which is given @'RawInstr'@s and outputs
-- ordinary @'Instr'@s.
--
-- Alternatively, if an instruction could not be properly decoded, an
-- @'Invalid'@ value is returned.
data Instr
  = Instr16 Instr16
  | Instr32 Instr32
  | Invalid
  deriving (Eq, Show)

-- | Second-stage of the instruction decoder: decodes a @'RawInstr'@ into the
-- corresponding 16 or 32 bit instruction.
decodeInstr :: RawInstr -> Instr

-- The first case, 16-bit instructions, are relatively easy, but still tedious.
decodeInstr (RawInstr16 RawPiece{..}) = either id Instr16 result where
  result = case rawPieceOpClass of
    -- ALU instructions
    0b00 -> case rawPiece1 of
      0b0000 -> Right $ NOP16  rawPiece2 (rawPiece3 ++# rawPiece4)
      0b0001 -> Right $ ADD16  rawPiece2 rawPiece3 rawPiece4
      0b0010 -> Right $ SUB16  rawPiece2 rawPiece3 rawPiece4
      0b0011 -> Right $ AND16  rawPiece2 rawPiece3 rawPiece4
      0b0100 -> Right $ OR16   rawPiece2 rawPiece3 rawPiece4
      0b0101 -> Right $ XOR16  rawPiece2 rawPiece3 rawPiece4
      0b0110 -> Right $ ASR16  rawPiece2 rawPiece3 rawPiece4
      0b0111 -> Right $ LSL16  rawPiece2 rawPiece3 rawPiece4
      0b1000 -> Right $ LSR16  rawPiece2 rawPiece3 rawPiece4
      0b1001 -> Right $ MOV16  rawPiece2 rawPiece3
      0b1010 -> Right $ ADDI16 rawPiece2 rawPiece3 rawPiece4
      0b1011 -> Right $ SUBI16 rawPiece2 rawPiece3 rawPiece4
      0b1100 -> Right $ ASRI16 rawPiece2 rawPiece3 rawPiece4
      0b1101 -> Right $ LSLI16 rawPiece2 rawPiece3 rawPiece4
      0b1110 -> Right $ LSRI16 rawPiece2 rawPiece3 rawPiece4
      0b1111 -> Right $ MOVI16 rawPiece2 (rawPiece3 ++# rawPiece4)
      _      -> Left  $ Invalid

    -- Load/store instructions
    0b01 -> case rawPiece1 of
      0b0000 -> Right $ LDB16 None    rawPiece2 rawPiece3 rawPiece4
      0b0100 -> Right $ LDW16 None    rawPiece2 rawPiece3 rawPiece4
      0b0001 -> Right $ LDB16 PostInc rawPiece2 rawPiece3 rawPiece4
      0b0101 -> Right $ LDW16 PostInc rawPiece2 rawPiece3 rawPiece4
      0b0010 -> Right $ LDB16 PreDec  rawPiece2 rawPiece3 rawPiece4
      0b0110 -> Right $ LDW16 PreDec  rawPiece2 rawPiece3 rawPiece4
      0b1000 -> Right $ STB16 None    rawPiece2 rawPiece3 rawPiece4
      0b1100 -> Right $ STW16 None    rawPiece2 rawPiece3 rawPiece4
      0b1001 -> Right $ STB16 PostInc rawPiece2 rawPiece3 rawPiece4
      0b1101 -> Right $ STW16 PostInc rawPiece2 rawPiece3 rawPiece4
      0b1010 -> Right $ STB16 PreDec  rawPiece2 rawPiece3 rawPiece4
      0b1110 -> Right $ STW16 PreDec  rawPiece2 rawPiece3 rawPiece4
      _      -> Left  $ Invalid

    -- Branch instructions
    0b10 -> case rawPiece1 of
      0b0000 -> Right $ BRA16  (rawPiece2 ++# rawPiece3 ++# rawPiece4)
      0b0001 -> Right $ BAL16  (rawPiece2 ++# rawPiece3) rawPiece4
      0b0010 -> Right $ BEQ16  rawPiece2 rawPiece3 rawPiece4
      0b0011 -> Right $ BNE16  rawPiece2 rawPiece3 rawPiece4
      0b0100 -> Right $ BLTS16 rawPiece2 rawPiece3 rawPiece4
      0b0101 -> Right $ BLES16 rawPiece2 rawPiece3 rawPiece4
      0b0110 -> Right $ BLTU16 rawPiece2 rawPiece3 rawPiece4
      0b0111 -> Right $ BLEU16 rawPiece2 rawPiece3 rawPiece4
      0b1000 -> Right $ JMP16  rawPiece2
      0b1001 -> Right $ JAL16  rawPiece2 rawPiece4
      0b1010 -> Right $ JEQ16  rawPiece2 rawPiece3 rawPiece4
      0b1011 -> Right $ JNE16  rawPiece2 rawPiece3 rawPiece4
      0b1100 -> Right $ JLTS16 rawPiece2 rawPiece3 rawPiece4
      0b1101 -> Right $ JLES16 rawPiece2 rawPiece3 rawPiece4
      0b1110 -> Right $ JLTU16 rawPiece2 rawPiece3 rawPiece4
      0b1111 -> Right $ JLEU16 rawPiece2 rawPiece3 rawPiece4
      _      -> Left  $ Invalid

    -- Miscellaneous instructions
    0b11 -> case rawPiece1 of
      0b0000 -> Right $ RTE16 rawPiece2
      _      -> Left  $ Invalid

    -- Done
    _ -> Left Invalid

-- The 32-bit case is even more tedious to handle, and requires merging
-- values from both p1 and p2.
decodeInstr (RawInstr32 p1 p2 imm extra) = either id Instr32 result where
  result = case rawPieceOpClass p2 of
    -- ALU instructions
    0b00 -> case rawPiece1 p2 of
      0b0000 -> Left Invalid
      0b0001 -> Right $ ADD32 (rawPiece2 p1 ++# rawPiece2 p2)
                              (rawPiece3 p1 ++# rawPiece3 p2)
                              (rawPiece4 p1 ++# rawPiece4 p2)
      _  -> Left Invalid

    -- Load/store instructions
    0b01 -> case rawPiece1 p2 of
      _ -> Left Invalid

    -- Branch/jump instructions
    0b10 -> case rawPiece1 p2 of
      0b0011 -> Right $ BNE32 (rawPiece1 p1 ++# rawPiece2 p1 ++# rawPiece2 p2)
                              (rawPiece3 p1 ++# rawPiece3 p2)
                              (rawPiece4 p1 ++# rawPiece4 p2)
      _ -> Left Invalid
      
    -- Done
    _ -> Left Invalid

--------------------------------------------------------------------------------
-- Instruction decoder: full interface

-- | Determine if an @'Instr'@ is 16-bit.
isInstr16 :: Instr -> Bool
isInstr16 (Instr16 _) = True
isInstr16 _           = False

-- | Determine if an @'Instr'@ is 32-bit.
isInstr32 :: Instr -> Bool
isInstr32 (Instr32 _) = True
isInstr32 _           = False

-- | Second-stage of the instruction decoder: decodes a @'RawInstr'@ into the
-- | Decode a 32-bit value into an AAP instruction.
decode :: BitVector 32 -> Instr
decode x = decodeInstr (decodeRaw x)
