{-# LANGUAGE DataKinds #-}
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
module AAP.Sim.Disassemble
  ( decode16
  , decode32
  , prettyInstr
  ) where
import Prelude
import Data.List             ( intercalate )
import GHC.TypeLits          ( KnownNat )

import AAP.Types
import AAP.Decoder

--------------------------------------------------------------------------------
-- Disassembler

-- | Print out an instruction in a prettier form.
prettyInstr :: Instr -> String
prettyInstr instr = case instr of
  -- ALU instructions
  AluInstr (NOP rd i)      -> op "nop"  [ regify rd, immify i ]
  AluInstr (ADD rd ra rb)  -> op "add"  [ regify rd, regify ra, regify rb ]
  AluInstr (SUB rd ra rb)  -> op "sub"  [ regify rd, regify ra, regify rb ]
  AluInstr (AND rd ra rb)  -> op "and"  [ regify rd, regify ra, regify rb ]
  AluInstr (OR  rd ra rb)  -> op "or"   [ regify rd, regify ra, regify rb ]
  AluInstr (XOR rd ra rb)  -> op "xor"  [ regify rd, regify ra, regify rb ]
  AluInstr (ASR rd ra rb)  -> op "asr"  [ regify rd, regify ra, regify rb ]
  AluInstr (LSL rd ra rb)  -> op "lsl"  [ regify rd, regify ra, regify rb ]
  AluInstr (LSR rd ra rb)  -> op "lsr"  [ regify rd, regify ra, regify rb ]
  AluInstr (MOV rd ra)     -> op "mov"  [ regify rd, regify ra ]
  AluInstr (ADDI rd ra rb) -> op "addi" [ regify rd, regify ra, immify rb ]
  AluInstr (SUBI rd ra rb) -> op "subi" [ regify rd, regify ra, immify rb ]
  AluInstr (ASRI rd ra rb) -> op "asri" [ regify rd, regify ra, immify' (+1) rb ]
  AluInstr (LSLI rd ra rb) -> op "lsli" [ regify rd, regify ra, immify' (+1) rb ]
  AluInstr (LSRI rd ra rb) -> op "lsri" [ regify rd, regify ra, immify' (+1) rb ]
  AluInstr (MOVI rd i)     -> op "movi" [ regify rd, immify i ]

  AluInstr (ADDC rd ra rb) -> op "addc" [ regify rd, regify ra, regify rb ]
  AluInstr (SUBC rd ra rb) -> op "subc" [ regify rd, regify ra, regify rb ]
  AluInstr (ANDI rd ra rb) -> op "andi" [ regify rd, regify ra, immify rb ]
  AluInstr (ORI  rd ra rb) -> op "ori"  [ regify rd, regify ra, immify rb ]
  AluInstr (XORI rd ra rb) -> op "xori" [ regify rd, regify ra, immify rb ]

  -- Load/Store instructions
  StoreInstr (LDB m rd ra s) -> loadop  "ldb" m rd ra s
  StoreInstr (LDW m rd ra s) -> loadop  "ldw" m rd ra s
  StoreInstr (STB m rd s ra) -> storeop "stb" m rd s ra
  StoreInstr (STW m rd s ra) -> storeop "stw" m rd s ra

  -- Branch/jump instructions
  BranchInstr (BRA  s)        -> op "bra"  [ simmify s ]
  BranchInstr (BAL  s rb)     -> op "bal"  [ simmify s, regify rb ]
  BranchInstr (BEQ  s ra rb)  -> op "beq"  [ simmify s, regify ra, regify rb ]
  BranchInstr (BNE  s ra rb)  -> op "bne"  [ simmify s, regify ra, regify rb ]
  BranchInstr (BLTS s ra rb)  -> op "blts" [ simmify s, regify ra, regify rb ]
  BranchInstr (BLES s ra rb)  -> op "bles" [ simmify s, regify ra, regify rb ]
  BranchInstr (BLTU s ra rb)  -> op "bltu" [ simmify s, regify ra, regify rb ]
  BranchInstr (BLEU s ra rb)  -> op "bleu" [ simmify s, regify ra, regify rb ]
  BranchInstr (JMP  rd)       -> op "jmp"  [ regify rd ]
  BranchInstr (JAL  rd rb)    -> op "jal"  [ regify rd, regify rb ]
  BranchInstr (JEQ  rd ra rb) -> op "jeq"  [ regify rd, regify ra, regify rb ]
  BranchInstr (JNE  rd ra rb) -> op "jne"  [ regify rd, regify ra, regify rb ]
  BranchInstr (JLTS rd ra rb) -> op "jlts" [ regify rd, regify ra, regify rb ]
  BranchInstr (JLES rd ra rb) -> op "jles" [ regify rd, regify ra, regify rb ]
  BranchInstr (JLTU rd ra rb) -> op "jltu" [ regify rd, regify ra, regify rb ]
  BranchInstr (JLEU rd ra rb) -> op "jleu" [ regify rd, regify ra, regify rb ]

  BranchInstr (JMPL  rd)       -> op "jmpl"  [ regify rd ]
  BranchInstr (JALL  rd rb)    -> op "jall"  [ regify rd, regify rb ]
  BranchInstr (JEQL  rd ra rb) -> op "jeql"  [ regify rd, regify ra, regify rb ]
  BranchInstr (JNEL  rd ra rb) -> op "jnel"  [ regify rd, regify ra, regify rb ]
  BranchInstr (JLTSL rd ra rb) -> op "jltsl" [ regify rd, regify ra, regify rb ]
  BranchInstr (JLESL rd ra rb) -> op "jlesl" [ regify rd, regify ra, regify rb ]
  BranchInstr (JLTUL rd ra rb) -> op "jltul" [ regify rd, regify ra, regify rb ]
  BranchInstr (JLEUL rd ra rb) -> op "jleul" [ regify rd, regify ra, regify rb ]

  -- Miscellaneous instructions
  MiscInstr (RTE rd) -> op "rte" [ regify rd ]

  Invalid -> "INVALID INSTRUCTION!!!"

--------------------------------------------------------------------------------
-- Utilities

-- | Print out some operand
op :: String -> [String] -> String
op name args = concat [ name, " ", intercalate ", " args ]

loadop :: (KnownNat s, KnownNat s', KnownNat s'')
       => String -> MemOpClass -> Reg r s -> Reg r' s' -> Imm 'S s'' -> String
loadop name None rd ra s =
  concat [ name, " ", regify rd, ", [", regify ra, ", ", simmify s, "]" ]
loadop name PostInc rd ra s =
  concat [ name, " ", regify rd, ", [", regify ra, "+, ", simmify s, "]" ]
loadop name PreDec rd ra s =
  concat [ name, " ", regify rd, ", [-", regify ra,", ", simmify s, "]" ]

storeop :: (KnownNat s, KnownNat s', KnownNat s'')
        => String -> MemOpClass -> Reg r s -> Imm 'S s' -> Reg r' s'' -> String
storeop name None rd s ra =
  concat [ name, " [", regify rd, ", ", simmify s, "], ", regify ra ]
storeop name PostInc rd s ra =
  concat [ name, " [", regify rd, "+, ", simmify s, "], ", regify ra ]
storeop name PreDec rd s ra =
  concat [ name, " [-", regify rd, ", ", simmify s, "], ", regify ra ]


-- | Print out some unsigned immediate value.
immify' :: KnownNat n => (Integer -> Integer) -> Imm 'I n -> String
immify' k = show . k . toInteger . immUnsigned

immify :: KnownNat n => Imm 'I n -> String
immify = immify' id

-- | Print out some /signed/ immediate value.
simmify :: forall n. KnownNat n => Imm 'S n -> String
simmify = show . toInteger . immSigned where

-- | Print out some value like a register.
regify :: KnownNat n => Reg r n -> String
regify = ("$r" ++) . show . toInteger
