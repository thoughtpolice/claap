{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : AAP.Sim.Disassemble
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AAP disassembler.
--
-- This module is (obviously) not synthesizable.
--
module AAP.Sim.Disassembler
  ( prettyInstr
  ) where
import Prelude
import Data.List (intercalate)
import GHC.TypeLits (KnownNat)

import CLaSH.Class.BitPack (BitPack(..))
import CLaSH.Sized.Signed (Signed)
import CLaSH.Sized.BitVector (BitVector)

import AAP.Decoder

--------------------------------------------------------------------------------
-- Simple interface

--------------------------------------------------------------------------------
-- Disassembler

-- | Print out an instruction in a prettier form.
prettyInstr :: Instr -> String
-- 16-bit instructions
prettyInstr (Instr16 instr) = result where
  result = case instr of
    -- ALU instructions
    NOP16 rd i      -> op "nop"  [ regify rd, immify i ]
    ADD16 rd ra rb  -> op "add"  (map regify [ rd, ra, rb ])
    SUB16 rd ra rb  -> op "sub"  (map regify [ rd, ra, rb ])
    AND16 rd ra rb  -> op "and"  (map regify [ rd, ra, rb ])
    OR16  rd ra rb  -> op "or"   (map regify [ rd, ra, rb ])
    XOR16 rd ra rb  -> op "xor"  (map regify [ rd, ra, rb ])
    ASR16 rd ra rb  -> op "asr"  (map regify [ rd, ra, rb ])
    LSL16 rd ra rb  -> op "lsl"  (map regify [ rd, ra, rb ])
    LSR16 rd ra rb  -> op "lsr"  (map regify [ rd, ra, rb ])
    MOV16 rd ra     -> op "mov"  (map regify [ rd, ra ])
    ADDI16 rd ra rb -> op "addi" (map regify [ rd, ra, rb ])
    SUBI16 rd ra rb -> op "subi" (map regify [ rd, ra, rb ])
    ASRI16 rd ra rb -> op "asri" (map regify [ rd, ra, rb ])
    LSLI16 rd ra rb -> op "lsli" (map regify [ rd, ra, rb ])
    LSRI16 rd ra rb -> op "lsri" (map regify [ rd, ra, rb ])
    MOVI16 rd i     -> op "movi" [ regify rd, immify i ]

    -- Load/Store instructions
    LDB16 memop rd ra s -> loadop  "ldb" memop rd ra s
    LDW16 memop rd ra s -> loadop  "ldw" memop rd ra s
    STB16 memop rd ra s -> storeop "stb" memop rd ra s
    STW16 memop rd ra s -> storeop "stw" memop rd ra s

    -- Branch/jump instructions
    BRA16  s        -> op "bra"  [ simmify s ]
    BAL16  s rb     -> op "bal"  [ simmify s, regify rb ]
    BEQ16  s ra rb  -> op "beq"  [ simmify s, regify ra, regify rb ]
    BNE16  s ra rb  -> op "bne"  [ simmify s, regify ra, regify rb ]
    BLTS16 s ra rb  -> op "blts" [ simmify s, regify ra, regify rb ]
    BLES16 s ra rb  -> op "bles" [ simmify s, regify ra, regify rb ]
    BLTU16 s ra rb  -> op "bltu" [ simmify s, regify ra, regify rb ]
    BLEU16 s ra rb  -> op "bleu" [ simmify s, regify ra, regify rb ]
    JMP16  rd       -> op "jmp"  [ regify rd ]
    JAL16  rd rb    -> op "jal"  (map regify [ rd, rb ])
    JEQ16  rd ra rb -> op "jeq"  (map regify [ rd, ra, rb ])
    JNE16  rd ra rb -> op "jne"  (map regify [ rd, ra, rb ])
    JLTS16 rd ra rb -> op "jlts" (map regify [ rd, ra, rb ])
    JLES16 rd ra rb -> op "jles" (map regify [ rd, ra, rb ])
    JLTU16 rd ra rb -> op "jltu" (map regify [ rd, ra, rb ])
    JLEU16 rd ra rb -> op "jleu" (map regify [ rd, ra, rb ])

    -- Miscellaneous instructions
    RTE16 rd -> op "rte" [ regify rd ]

-- 32-bit instructions
prettyInstr (Instr32 instr) = result where
  result = case instr of
    -- ALU instructions
    NOP32 rd i      -> op "nop" [ regify rd, immify i ]
    ADD32 rd ra rb  -> op "add" [ regify rd, regify ra, regify rb ]
    SUB32 rd ra rb  -> op "sub"  (map regify [ rd, ra, rb ])
    AND32 rd ra rb  -> op "and"  (map regify [ rd, ra, rb ])
    OR32  rd ra rb  -> op "or"   (map regify [ rd, ra, rb ])
    XOR32 rd ra rb  -> op "xor"  (map regify [ rd, ra, rb ])
    ASR32 rd ra rb  -> op "asr"  (map regify [ rd, ra, rb ])
    LSL32 rd ra rb  -> op "lsl"  (map regify [ rd, ra, rb ])
    LSR32 rd ra rb  -> op "lsr"  (map regify [ rd, ra, rb ])
    MOV32 rd ra     -> op "mov"  [ regify rd, regify ra ]
    ADDI32 rd ra rb -> op "addi" [ regify rd, regify ra, regify rb ]
    SUBI32 rd ra rb -> op "subi" [ regify rd, regify ra, regify rb ]
    ASRI32 rd ra rb -> op "asri" (map regify [ rd, ra, rb ])
    LSLI32 rd ra rb -> op "lsli" (map regify [ rd, ra, rb ])
    LSRI32 rd ra rb -> op "lsri" (map regify [ rd, ra, rb ])
    MOVI32 rd i     -> op "movi" [ regify rd, immify i ]

    ADDC32 rd ra rb -> op "addc" (map regify [ rd, ra, rb ])
    SUBC32 rd ra rb -> op "subc" (map regify [ rd, ra, rb ])
    ANDI32 rd ra rb -> op "andi" [ regify rd, regify ra, regify rb ]
    ORI32  rd ra rb -> op "ori"  [ regify rd, regify ra, regify rb ]
    XORI32 rd ra rb -> op "xori" [ regify rd, regify ra, regify rb ]

    -- Load/Store instructions
    LDB32 memop rd ra s -> loadop  "ldb" memop rd ra s
    LDW32 memop rd ra s -> loadop  "ldw" memop rd ra s
    STB32 memop rd ra s -> storeop "stb" memop rd ra s
    STW32 memop rd ra s -> storeop "stw" memop rd ra s

    -- Branch/jump instructions
    BRA32  s        -> op "bra"  [ simmify s ]
    BAL32  s rb     -> op "bal"  [ simmify s, regify rb ]
    BEQ32  s ra rb  -> op "beq"  [ simmify s, regify ra, regify rb ]
    BNE32  s ra rb  -> op "bne"  [ simmify s, regify ra, regify rb ]
    BLTS32 s ra rb  -> op "blts" [ simmify s, regify ra, regify rb ]
    BLES32 s ra rb  -> op "bles" [ simmify s, regify ra, regify rb ]
    BLTU32 s ra rb  -> op "bltu" [ simmify s, regify ra, regify rb ]
    BLEU32 s ra rb  -> op "bleu" [ simmify s, regify ra, regify rb ]
    JMP32  rd       -> op "jmp"  [ regify rd ]
    JAL32  rd rb    -> op "jal"  (map regify [ rd, rb ])
    JEQ32  rd ra rb -> op "jeq"  (map regify [ rd, ra, rb ])
    JNE32  rd ra rb -> op "jne"  (map regify [ rd, ra, rb ])
    JLTS32 rd ra rb -> op "jlts" (map regify [ rd, ra, rb ])
    JLES32 rd ra rb -> op "jles" (map regify [ rd, ra, rb ])
    JLTU32 rd ra rb -> op "jltu" (map regify [ rd, ra, rb ])
    JLEU32 rd ra rb -> op "jleu" (map regify [ rd, ra, rb ])

    JMPL32  rd       -> op "jmpl"  [ regify rd ]
    JALL32  rd rb    -> op "jall"  (map regify [ rd, rb ])
    JEQL32  rd ra rb -> op "jeql"  (map regify [ rd, ra, rb ])
    JENL32  rd ra rb -> op "jenl"  (map regify [ rd, ra, rb ])
    JLTSL32 rd ra rb -> op "jltsl" (map regify [ rd, ra, rb ])
    JLESL32 rd ra rb -> op "jlesl" (map regify [ rd, ra, rb ])
    JLTUL32 rd ra rb -> op "jltul" (map regify [ rd, ra, rb ])
    JLEUL32 rd ra rb -> op "jleul" (map regify [ rd, ra, rb ])

-- Invalid instructions
prettyInstr Invalid = "INVALID INSTRUCTION"

--------------------------------------------------------------------------------
-- Utilities

-- | Print out some operand
op :: String -> [String] -> String
op name args = concat [ name, " ", intercalate ", " args ]

loadop name None rd ra s =
  concat [ name, " ", regify rd, ", (", regify ra, ", ", immify s, ")" ]
loadop name PostInc rd ra s =
  concat [ name, " ", regify rd, ", (", regify ra, "+, ", immify s, ")" ]
loadop name PreDec rd ra s =
  concat [ name, " ", regify rd, ", (-", regify ra,", ", immify s, ")" ]

storeop name None rd ra s =
  concat [ name, " (", regify rd, ", ", immify s, "), ", regify ra ]
storeop name PostInc rd ra s =
  concat [ name, " (", regify rd, "+, ", immify s, "), ", regify ra ]
storeop name PreDec rd ra s =
  concat [ name, " (-", regify rd, ", ", immify s, "), ", regify ra ]


-- | Print out some value like an immediate value
immify :: KnownNat n => BitVector n -> String
immify = show . toInteger

simmify :: forall n. KnownNat n => BitVector n -> String
simmify = show . toInteger . conv where
  conv = unpack :: BitVector n -> Signed n

-- | Print out some value like a register.
regify :: KnownNat n => BitVector n -> String
regify = ("$r" ++) . show . toInteger
