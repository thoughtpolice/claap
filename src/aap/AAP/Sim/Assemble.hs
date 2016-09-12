{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : AAP.Sim.Disassemble
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AAP assembler, as an embedded domain specific language inside Haskell.
--
module AAP.Sim.Assemble
  ( -- * AAP Assembler Monad
    ASM          -- :: * -> *
  , runASM       -- :: Int -> ASM a -> [Instr]
  , assemble     -- :: Int -> ASM a -> [Word8]
--, assembleFile -- :: FilePath -> Int -> ASM a -> IO ()

    -- ** Primitives
  , Label  -- :: *
  , label  -- :: ASM Label
  , end    -- :: ASM ()

    -- ** Instruction encoder
  , encode -- :: [Instr] -> [Word8]

    -- * Instruction set
    -- ** ALU instructions
    -- *** Register-to-register operations
  , add,  sub,  and,  or,  xor,  asr,  lsl,  lsr,  mov
    -- *** Register/immediate operations
  , addi, subi, andi, ori, xori, asri, lsli, lsri, movi
  , addc, subc

    -- ** Store/load instructions
    -- *** Load instructions
  , ldb, ldw
    -- *** Store instructions
  , stb, stw

    -- ** Branch instructions
  , bra, bal, beq, bne, blts, bles, bltu, bleu
  , jmp, jal, jeq, jne, jlts, jles, jltu, jleu
    -- *** Long-jump instructions
  , jmpl, jall, jeql, jnel, jltsl, jlesl, jltul, jleul

    -- ** Miscellaneous instructions
  , nop
  , rte

    -- * Register file names
    -- ** Mnemonics for specific registers
    -- | The following names are mnemonics which refer to other registers in the
    -- register file for making code easier to read.
  , lr, sp

    -- ** Full set of register names
    -- | You can use the following names to refer to individual registers inside
    -- the AAP register file (the internal definitions of these names should be
    -- considered unstable and prone to change).
  , r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,  r8,  r9,  r10, r11, r12, r13, r14, r15
  , r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31
  , r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47
  , r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62, r63
  ) where
import Prelude hiding (or, and)
import CLaSH.Sized.BitVector
import CLaSH.Prelude.BitIndex
import CLaSH.Promoted.Nat.Literals

--import qualified Data.ByteString as B (writeFile, pack)
import Data.Word (Word8)

import AAP.Types
import AAP.Decoder

import Control.Monad.Fix

--------------------------------------------------------------------------------
-- Assembler Monad framework

-- | A label. Use @'label'@ to generate these, and use the various included
-- jump/branch instructions to refer to each label.
newtype Label = Label { _unLabel :: Int }

data ASM' w a = ASM' { unASM' :: Label -> ([w], a) }

instance Functor (ASM' w) where
  fmap k (ASM' f) = ASM' $ \l ->
    let (w, a) = f l
    in (w, k a)

instance Applicative (ASM' w) where
  pure x                = ASM' $ const ([], x)
  (ASM' f) <*> (ASM' g) = ASM' $ \(Label l) ->
    let (w0, k) = f (Label l)
        (w1, a) = g (Label $! l + length w0)
    in (w0 ++ w1, k a)

instance Monad (ASM' w) where
  return x = ASM' $ const ([], x)
  f >>= g  = ASM' $ \(Label l) ->
    let (w0, a) = unASM' f     (Label l)
        (w1, b) = unASM' (g a) (Label $! l + length w0)
    in (w0 ++ w1, b)

instance MonadFix (ASM' w) where
  mfix f = ASM' $ \l ->
    let (w, a) = unASM' (f a) l
    in (w, a)

runASM' :: Int -> ASM' w a -> [w]
runASM' n act = fst (unASM' act (Label n))

tell' :: [w] -> ASM' w ()
tell' x = ASM' $ const (x, ())

label' :: ASM' w Label
label' = ASM' $ \l -> ([], l)

--------------------------------------------------------------------------------
-- Assembler monad, specialized to the AAP instruction set.

-- | Assembler Monad, representing an embedded language for writing AAP assembly
-- code with label support.
--
-- Because @'ASM'@ is an instance of @'MonadFix'@, you can use /recursive do/,
-- allowing you to refer to labels declared not just before, but also after the
-- declared instruction, like in a real assembler. See the @'label'@ primitive
-- below.
newtype ASM a = ASM { _unASM :: ASM' Instr a }
  deriving (Functor, Applicative, Monad, MonadFix)

-- | Run an @'ASM'@ action, disregard the return value, and return a list of
-- encoded instructions - the assembly instructions representing the embedded
-- code.
--
-- The first parameter determines the base address of where the code is expected
-- to be located. Note that AAP can use its dense, 16-bit instruction encoding
-- only for branch instructions that do not result in \"long\" jumps. The base
-- address will be encoded in the result of jumps. As a result, if you wish to
-- increase instruction density, it's preferrable to pick a low base address.
--
-- In order to turn a list of instructions into a stream of bytes suitable for
-- execution by the processor, you should use (preferrably) use @'assemble'@ or
-- @'encode'@. If you need to write the result to a file in some form, use
-- @'assembleFile'@.
runASM :: Int     -- ^ Base address
       -> ASM a   -- ^ Assembly action
       -> [Instr] -- ^ List of instructions
runASM base (ASM act) = runASM' base act

-- | Add an instruction to the assembly code stream.
--
-- __NOTE__: Private. Should not be exported.
tell :: [Instr] -> ASM ()
tell = ASM . tell'

-- | Create a label, return it, and run an @'ASM'@ action too. Note that the
-- action in itself has no influence on the resulting label or your ability to
-- reference labels declared before or after the current code; this parameter is
-- merely meant as a convenience to make code read better, along the lines of a
-- regular assembler.
--
--
-- In a regular assembler, you'd normally be used to indenting code after the
-- introduction of a label. It's the difference between:
--
-- @
-- lbl <- label (return ()
-- nop r0 1
-- ...
-- @
--
-- and:
--
-- @
-- lbl <- label $ do
--   nop r0 1
--   ...
-- @
--
--
label :: ASM r     -- ^ Action to run.
      -> ASM Label -- ^ The returned label.
label act = ASM label' <* act

-- | A simple @end@ statement. This does nothing, and is equivalent to @return
-- ()@. It is intended to make a piece of DSL code look better in situations
-- such as the following:
--
-- @
-- f :: ASM ()
-- f = mdo
--   ... lots of code ...
--   finish <- label $ do
--     nop r0 1
--   return ()
-- @
--
-- @
-- f :: ASM ()
-- f = mdo
--   ... lots of code ...
--   finish <- label $ do
--     nop r0 1
--   end
-- @
--
-- Because you cannot bind a name like @'finish'@ as the final statement in a
-- @do@ block, this makes the ending code look a bit nicer, and align it with
-- the type @'ASM' ()@.
end :: ASM ()
end = return ()

--
-- ALU
--

-- | Unsigned add.
add :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
add rd ra rb = tell [ AluInstr (ADD rd ra rb) ]

-- | Unsigned subtract
sub :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
sub rd ra rb = tell [ AluInstr (SUB rd ra rb) ]

-- | Bitwise AND.
and :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
and rd ra rb = tell [ AluInstr (AND rd ra rb) ]

-- | Bitwise OR.
or :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
or rd ra rb = tell [ AluInstr (OR rd ra rb) ]

-- | Bitwise exclusive OR.
xor :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
xor rd ra rb = tell [ AluInstr (XOR rd ra rb) ]

-- | Arithmetic shift right.
asr :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
asr rd ra rb = tell [ AluInstr (ASR rd ra rb) ]

-- | Logical shift left.
lsl :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
lsl rd ra rb = tell [ AluInstr (LSL rd ra rb) ]

-- | Logical shift right.
lsr :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
lsr rd ra rb = tell [ AluInstr (LSR rd ra rb) ]

-- | Move register to register.
mov :: Reg 'D 6 -> Reg 'A 6 -> ASM ()
mov rd ra = tell [ AluInstr (MOV rd ra) ]

-- Immediate versions

-- | Unsigned add immediate.
addi :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 10 -> ASM ()
addi rd ra imm = tell [ AluInstr (ADDI rd ra imm) ]

-- | Unsigned subtract immediate.
subi :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 10 -> ASM ()
subi rd ra imm = tell [ AluInstr (SUBI rd ra imm) ]

-- | Bitwise AND immediate.
andi :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 9 -> ASM ()
andi rd ra imm = tell [ AluInstr (ANDI rd ra imm) ]

-- | Bitwise OR immediate.
ori :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 9 -> ASM ()
ori rd ra imm = tell [ AluInstr (ORI rd ra imm) ]

-- | Bitwise exclusive OR immediate.
xori :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 9 -> ASM ()
xori rd ra imm = tell [ AluInstr (XORI rd ra imm) ]

-- | Arithmetic shift right immediate.
asri :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 6 -> ASM ()
asri rd ra imm = tell [ AluInstr (ASRI rd ra imm) ]

-- | Logical shift left immediate.
lsli :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 6 -> ASM ()
lsli rd ra imm = tell [ AluInstr (LSLI rd ra imm) ]

-- | Logical shift right immediate.
lsri :: Reg 'D 6 -> Reg 'A 6 -> Imm 'I 6 -> ASM ()
lsri rd ra imm = tell [ AluInstr (LSRI rd ra imm) ]

-- | Move immediate to register.
movi :: Reg 'D 6 -> Imm 'I 16 -> ASM ()
movi rd imm = tell [ AluInstr (MOVI rd imm) ]

-- Carry operations

-- | Add with carry.
addc :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
addc rd ra rb = tell [ AluInstr (ADDC rd ra rb) ]

-- | Subtract with carry.
subc :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
subc rd ra rb = tell [ AluInstr (SUBC rd ra rb) ]

--
-- Store/Load
--

-- | Indexed load byte (with postincrement or predecrement).
ldb :: MemOpClass -> Reg 'D 6 -> Reg 'A 6 -> Imm 'S 10 -> ASM ()
ldb cls rd ra imm = tell [ StoreInstr (LDB cls rd ra imm) ]

-- | Indexed load word (with postincrement or predecrement).
ldw :: MemOpClass -> Reg 'D 6 -> Reg 'A 6 -> Imm 'S 10 -> ASM ()
ldw cls rd ra imm = tell [ StoreInstr (LDW cls rd ra imm) ]

-- | Indexed store byte (with postincrement or predecrement).
stb :: MemOpClass -> Reg 'D 6 -> Imm 'S 10 -> Reg 'A 6 -> ASM ()
stb cls rd imm ra = tell [ StoreInstr (STB cls rd imm ra) ]

-- | Indexed store word (with postincrement or predecrement).
stw :: MemOpClass -> Reg 'D 6 -> Imm 'S 10 -> Reg 'A 6 -> ASM ()
stw cls rd imm ra = tell [ StoreInstr (STW cls rd imm ra) ]

--
-- Branch
--

-- | Relative branch.
bra :: Label -> ASM ()
bra (Label l) = tell [ BranchInstr $ BRA (fromIntegral l) ]

-- | Relative branch and link.
bal :: Label -> Reg 'B 6 -> ASM ()
bal (Label l) rb = tell [ BranchInstr $ BAL (fromIntegral l) rb ]

-- | Relative branch if equal.
beq :: Label -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
beq (Label l) ra rb = tell [ BranchInstr $ BEQ (fromIntegral l) ra rb ]

-- | Relative branch if not equal.
bne :: Label -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
bne (Label l) ra rb = tell [ BranchInstr $ BNE (fromIntegral l) ra rb ]

-- | Relative branch if signed less than.
blts :: Label -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
blts (Label l) ra rb = tell [ BranchInstr $ BLTS (fromIntegral l) ra rb ]

-- | Relative branch if signed less than or equal to.
bles :: Label -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
bles (Label l) ra rb = tell [ BranchInstr $ BLES (fromIntegral l) ra rb ]

-- | Relative branch if unsigned less than.
bltu :: Label -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
bltu (Label l) ra rb = tell [ BranchInstr $ BLTU (fromIntegral l) ra rb ]

-- | Relative branch if unsigned less than or equal to.
bleu :: Label -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
bleu (Label l) ra rb = tell [ BranchInstr $ BLEU (fromIntegral l) ra rb ]

-- | Absolute jump.
jmp :: Reg 'D 6 -> ASM ()
jmp rd = tell [ BranchInstr $ JMP rd ]

-- | Absolute jump and link.
jal :: Reg 'D 6 -> Reg 'B 6 -> ASM ()
jal rd rb = tell [ BranchInstr $ JAL rd rb ]

-- | Absolute jump if equal.
jeq :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jeq rd ra rb = tell [ BranchInstr $ JEQ rd ra rb ]

-- | Absolute jump if not equal.
jne :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jne rd ra rb = tell [ BranchInstr $ JNE rd ra rb ]

-- | Absolute jump if signed less than.
jlts :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jlts rd ra rb = tell [ BranchInstr $ JLTS rd ra rb ]

-- | Absolute jump if signed less than or equal to.
jles :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jles rd ra rb = tell [ BranchInstr $ JLES rd ra rb ]

-- | Absolute jump if unsigned less than.
jltu :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jltu rd ra rb = tell [ BranchInstr $ JLTU rd ra rb ]

-- | Absolute jump if unsigned less than or equal to.
jleu :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jleu rd ra rb = tell [ BranchInstr $ JLEU rd ra rb ]

-- Long variants.

-- | Absolute jump long.
jmpl :: Reg 'D 6 -> ASM ()
jmpl rd = tell [ BranchInstr $ JMPL rd ]

-- | Absolute jump long and link.
jall :: Reg 'D 6 -> Reg 'B 6 -> ASM ()
jall rd rb = tell [ BranchInstr $ JALL rd rb ]

-- | Absolute jump long if equal.
jeql :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jeql rd ra rb = tell [ BranchInstr $ JEQL rd ra rb ]

-- | Absolute jump long if not equal.
jnel :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jnel rd ra rb = tell [ BranchInstr $ JNEL rd ra rb ]

-- | Absolute jump long if signed less than.
jltsl :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jltsl rd ra rb = tell [ BranchInstr $ JLTSL rd ra rb ]

-- | Absolute jump long if signed less than or equal to.
jlesl :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jlesl rd ra rb = tell [ BranchInstr $ JLESL rd ra rb ]

-- | Absolute jump long if unsigned less than.
jltul :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jltul rd ra rb = tell [ BranchInstr $ JLTUL rd ra rb ]

-- | Absolute jump long if unsigned less than or equal to.
jleul :: Reg 'D 6 -> Reg 'A 6 -> Reg 'B 6 -> ASM ()
jleul rd ra rb = tell [ BranchInstr $ JLEUL rd ra rb ]

--
-- Miscellaneous
--

-- | No Operation. The most classic instruction of them all (except perhaps
-- @HALT AND CATCH FIRE@).
--
-- Despite the timeless moniker, this instruction can actually have multiple
-- effects when running inside e.g. simulators, such as writing values to the
-- terminal. This is to ease debugging and implementation.
--
-- The immediate argument determines what behavior is taken:
--
--   * __0__: Breakpoint.
--   * __1__: Do nothing.
--   * __2__: Exit with return code in @Rd@
--   * __3__: Write character in @Rd@ to standard output.
--   * __4__: Write character in @Rd@ to standard error.
--   * __All other values__: Do nothing. Future behavior not guaranteed.
--
-- All implementations should use @R0@ with immediate argument @0@ as a break
-- instruction, which should halt the processor.
--
-- All implementations should use @R0@ with immediate argument @1@ as a true
-- no-operation instruction.
--
-- The rationale behind this decision is that in an erroneous program, the most
-- likely value to be encountered as a random instruction is zero, which will
-- stop the processor.
--
nop :: Reg 'D 6 -- ^ Input register
    -> Int      -- ^ Immediate argument: Operation Mode
    -> ASM ()
nop rd i = tell [ AluInstr (NOP rd (fromIntegral i)) ]

-- | Return from Exception. This opcode is not fully defined, pending
-- finalization of the exception mechanism for AAP.
--
-- Outcome:
--
-- @
-- PC <- Rd
-- @
rte :: Reg 'D 3 -- ^ Input register, assigned to PC
    -> ASM ()
rte rd = tell [ MiscInstr $ RTE rd ]

--------------------------------------------------------------------------------
-- Instruction encoder

-- | Encode an @'ASM'@ action into a stream of bytes, suitable for execution by
-- the processor.
{--
assemble :: Int     -- ^ Base address.
         -> ASM a   -- ^ Assembly code to assemble into bytes.
         -> [Word8] -- ^ Encoded instruction stream.
--}
assemble base act = encode base (runASM base act)

{--
-- | Output of @'assembleFile'@. This is used to specify how the resulting file
-- should be written.
data AssemblerOutput
  = RawBytes   FilePath
  -- ^ Encode the output directly as a string of raw bytes, with no padding, and
  -- stuff the bytes into the specified @'FilePath'@.

  | HexEncoded Int FilePath
  -- ^ Encode the result in a @.hex@ data file format which can be read by CLaSH
  -- and HDL synthesis tools, in order to embed the resulting assembly code into
  -- a design which will be synthesized (for example, to embed initialization
  -- firmware with the processor).
  --
  -- The first argument is the resulting size of the output file. This needs to
  -- be statically known for the synthesis tools, in order to initialize
  -- RAMs/ROMs properly. If the encoded assembly is too large to fit in the
  -- specified buffer, an error is thrown.
  --
  -- The second argument is the @'FilePath'@ to write the output to. The output
  -- file will be written in terms of 16-bit word values.
  --
  -- For information on the BNF grammar of the output .hex file format, see
  -- @'CLaSH.Prelude.ROM.File'@.
--}

{--
-- | Encode an @'ASM'@ action into a stream of bytes, and write those bytes into
-- a file directly.
assembleFile :: FilePath -- ^ File to write
             -> Int      -- ^ Base address.
             -> ASM a    -- ^ Assembly code to assemble.
             -> IO ()
assembleFile file base act = B.writeFile file (B.pack bytes) where
  bytes = assemble base act
--}

-- | Encode a list of instructions into a stream of bytes, suitable for
-- execution by the processor. This performs relocation on the branch labels,
-- and ensures that the smallest encodings are chosen for each instruction.
--
-- The first argument is the base address. The second is the stream of
-- instructions to encode. This should generally be the same base address passed
-- to @'runASM'@ or otherwise specified when generating your code.
--
-- For a more convenient interface that avoids the possibility of messing this
-- up, see @'assemble'@.

{--
encode :: Int     -- ^ Base address.
       -> [Instr] -- ^ Instructions to encode.
       -> [Word8] -- ^ Encoded instruction stream.
--}
encode base instrs = tagged
  where
    -- Tag every instruction with the branch offset of where it's trying to
    -- jump, if applicable.
    tagged :: [(Maybe Int, [Word8])]
    tagged = map (\x -> (look x, mangle x)) instrs

    -- Find out where each branch instruction to a relative label is pointing
    -- to, and grab it.
    look :: Instr -> Maybe Int
    look (BranchInstr (BRA i))      = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BAL i _))    = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BEQ i _ _))  = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BNE i _ _))  = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BLTS i _ _)) = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BLES i _ _)) = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BLTU i _ _)) = Just (abs $ base - fromIntegral i)
    look (BranchInstr (BLEU i _ _)) = Just (abs $ base - fromIntegral i)
    look _                          = Nothing

    mangle :: Instr -> [Word8]
    mangle Invalid   = error "AAP.Sim.Assemble.encode: mangle failed - Invalid"

    ----------------------------------------------------------------------------
    -- ALU instructions

    mangle (AluInstr (NOP (regBV -> rd) (immUBV -> imm)))
      -- short form
      | allTrue [ 7 >= rd, 63 >= imm ]
      = short 0b00 0b0000 (slice3 rd) (slice d5 d3 imm) (slice d2 d0 imm)

      -- long form
      | otherwise
      = []
      {--
        map fromIntegral [ 0b0000000       ++# (rdt ! 2)
                         , slice d1 d0 rdt ++# it
                         , 0b1000000       ++# (rdb ! 2)
                         , slice d1 d0 rdb ++# ib
                         ]
      where
        (rdt, rdb) = splitR rd
        (it,  ib)  = split  imm :: (BitVector 6, BitVector 6)
--}
    mangle (AluInstr (ADD (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0001 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (SUB (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0010 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (AND (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0011 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (OR (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0100 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (XOR (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0101 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (ASR (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0110 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (LSL (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b0111 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (LSR (regBV -> rd) (regBV -> ra) (regBV -> rb)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= rb ]
      = short 0b00 0b1000 (slice3 rd) (slice3 ra) (slice3 rb)
      | otherwise
      = []

    mangle (AluInstr (MOV (regBV -> rd) (regBV -> ra)))
      | allTrue [ 7 >= rd, 7 >= ra ]
      = short 0b00 0b1001 (slice3 rd) (slice3 ra) 0
      | otherwise
      = []

    mangle (AluInstr (ADDI (regBV -> rd) (regBV -> ra) (immUBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= imm ]
      = short 0b00 0b1010 (slice3 rd) (slice3 ra) (slice3 imm)
      | otherwise
      = []

    mangle (AluInstr (SUBI (regBV -> rd) (regBV -> ra) (immUBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 7 >= imm ]
      = short 0b00 0b1011 (slice3 rd) (slice3 ra) (slice3 imm)
      | otherwise
      = []

    mangle (AluInstr (ASRI (regBV -> rd) (regBV -> ra) (immUBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 8 >= imm, imm >= 1 ]
      = short 0b00 0b1100 (slice3 rd) (slice3 ra) (slice3 (imm-1))
      | otherwise
      = []

    mangle (AluInstr (LSLI (regBV -> rd) (regBV -> ra) (immUBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 8 >= imm, imm >= 1 ]
      = short 0b00 0b1101 (slice3 rd) (slice3 ra) (slice3 (imm-1))
      | otherwise
      = []

    mangle (AluInstr (LSRI (regBV -> rd) (regBV -> ra) (immUBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 8 >= imm, imm >= 1 ]
      = short 0b00 0b1110 (slice3 rd) (slice3 ra) (slice3 (imm-1))
      | otherwise
      = []

    mangle (AluInstr (MOVI (regBV -> rd) (immUBV -> imm)))
      | allTrue [ 7 >= rd, 63 >= imm ]
      = short 0b00 0b1111 (slice3 rd) (slice d5 d3 imm) (slice d2 d0 imm)
      | otherwise
      = []

    ----------------------------------------------------------------------------
    -- Store/Load instructions

    mangle (StoreInstr x@(LDB _ (regBV -> rd) (regBV -> ra) (immSBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 3 >= imm, imm >= negate 4 ]
      = short 0b01 (memCls x) (slice3 rd) (slice3 ra) (slice3 imm)
      | otherwise
      = []
    mangle (StoreInstr x@(LDW _ (regBV -> rd) (regBV -> ra) (immSBV -> imm)))
      | allTrue [ 7 >= rd, 7 >= ra, 3 >= imm, imm >= negate 4 ]
      = short 0b01 (memCls x) (slice3 rd) (slice3 ra) (slice3 imm)
      | otherwise
      = []

    mangle (StoreInstr x@(STB _ (regBV -> rd) (immSBV -> imm) (regBV -> ra)))
      | allTrue [ 7 >= rd, 7 >= ra, 3 >= imm, imm >= negate 4 ]
      = short 0b01 (memCls x) (slice3 rd) (slice3 ra) (slice3 imm)
      | otherwise
      = []
    mangle (StoreInstr x@(STW _ (regBV -> rd) (immSBV -> imm) (regBV -> ra)))
      | allTrue [ 7 >= rd, 7 >= ra, 3 >= imm, imm >= negate 4 ]
      = short 0b01 (memCls x) (slice3 rd) (slice3 ra) (slice3 imm)
      | otherwise
      = []

    ----------------------------------------------------------------------------
    -- Branch instructions

    ----------------------------------------------------------------------------
    -- Miscellaneous instructions

    mangle (MiscInstr (RTE (regBV -> rd)))
      | allTrue [ 7 >= rd ]
      = short 0b11 0b0000 (slice3 rd) 0 0
      | otherwise
      = error "Assemble.encode.mangle: RTE doesn't have 32-bit form!"

    -- TODO FIXME: finish
--    mangle _
--      = []

    allTrue = all (== True)
    slice3  = slice d2 d0

    memCls (LDB None _ _ _)    = 0b0000
    memCls (LDW None _ _ _)    = 0b0100
    memCls (LDB PostInc _ _ _) = 0b0001
    memCls (LDW PostInc _ _ _) = 0b0101
    memCls (LDB PreDec _ _ _)  = 0b0010
    memCls (LDW PreDec _ _ _)  = 0b0110
    memCls (STB None _ _ _)    = 0b1000
    memCls (STW None _ _ _)    = 0b1100
    memCls (STB PostInc _ _ _) = 0b1001
    memCls (STW PostInc _ _ _) = 0b1101
    memCls (STB PreDec _ _ _)  = 0b1010
    memCls (STW PreDec _ _ _)  = 0b1110
    
    short :: BitVector 2
          -> BitVector 4
          -> BitVector 3
          -> BitVector 3
          -> BitVector 3
          -> [Word8]
    short cls op v1 v2 v3 = map fromIntegral [ x2, x1 ]
      where
        result   = low ++# cls ++# op ++# v1 ++# v2 ++# v3
        (x1, x2) = split result :: (BitVector 8, BitVector 8)

--------------------------------------------------------------------------------
-- Example

_ex1 :: ASM ()
_ex1 = mdo
  bra next

  start <- label $ do
    nop r0 1
    add r0 r1 r2
    sub r0 r1 r3
    bra finish

  next <- label $ do
    and r0 r1 r2
    bra start

  finish <- label $ do
    nop r0 1

  end

--------------------------------------------------------------------------------
-- Registers

r0,  r1,  r2,  r3,  r4,  r5,  r6,  r7,  r8,  r9,  r10, r11, r12, r13, r14, r15,
 r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31,
 r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47,
 r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62, r63
 :: Reg t 6

-- | Link register. Defined by the ABI to be equal to @'r0'@. This is a more
-- convenient mnemonic to use.
lr :: Reg t 6
lr = r0

-- | Stack pointer. Defined by the ABI to be equal to @'r1'@. This is a more
-- convenient mnemonic to use.
sp :: Reg t 6
sp = r1

-- Main names
r0  = 0
r1  = 1
r2  = 2
r3  = 3
r4  = 4
r5  = 5
r6  = 6
r7  = 7
r8  = 8
r9  = 9
r10 = 10
r11 = 11
r12 = 12
r13 = 13
r14 = 14
r15 = 15
r16 = 16
r17 = 17
r18 = 18
r19 = 19
r20 = 20
r21 = 21
r22 = 22
r23 = 23
r24 = 24
r25 = 25
r26 = 26
r27 = 27
r28 = 28
r29 = 29
r30 = 30
r31 = 31
r32 = 32
r33 = 33
r34 = 34
r35 = 35
r36 = 36
r37 = 37
r38 = 38
r39 = 39
r40 = 40
r41 = 41
r42 = 42
r43 = 43
r44 = 44
r45 = 45
r46 = 46
r47 = 47
r48 = 48
r49 = 49
r50 = 50
r51 = 51
r52 = 52
r53 = 53
r54 = 54
r55 = 55
r56 = 56
r57 = 57
r58 = 58
r59 = 59
r60 = 60
r61 = 61
r62 = 62
r63 = 63
