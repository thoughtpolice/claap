{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      : AAP.State
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AAP CPU state.
--
module AAP.State
  ( -- * CPU State type

    -- ** Register file
    CpuRegFile(..)
    -- *** Lenses
  , cpuRegRead, cpuRegWrite

    -- ** Data memory
  , CpuDataMem(..)
  , cpuDmemRead, cpuDmemWrite

    -- ** Main CPU state
  , CpuState(..) -- :: *
  , defaultCpuState
    -- *** Lenses
  , cpuPC
  , cpuCarry
  , cpuRegFile
  , cpuDmem

    -- * CPU Reader type
  , CpuInput(..)
    -- ** Lenses
  , cpuInInstr

  -- * CPU State Monad
  , CpuM         -- :: * -> *
  , runCpuM      -- :: CpuState -> CpuInput -> CpuM a -> (a, CpuState)

  -- * Mealy machine transfer conversion
  , cpuMealyT
  ) where
import CLaSH.Prelude

import Data.Tuple      ( swap )
import Control.Lens.TH ( makeLenses )

import Control.Monad.Reader
import Control.Monad.State

import AAP.PC
import AAP.Decoder
import AAP.Types

import AAP.RegFile

--------------------------------------------------------------------------------
-- CPU State

data CpuRegFile
  = CpuRegFile
    { _cpuRegRead  :: (Maybe RegReadAddr, Maybe RegReadAddr)
    , _cpuRegWrite :: RegValue -> RegValue -> RegFileWrite
    }

makeLenses ''CpuRegFile

data CpuDataMem
  = CpuDataMem
    { _cpuDmemRead  :: Maybe MemReadAddr
    , _cpuDmemWrite :: MemValue -> Maybe (MemWriteAddr, MemValue)
    }

makeLenses ''CpuDataMem

-- | AAP CPU state, carried from clock signal to clock signal as part of the
-- main mealy machine representing the execution engine. This is the \"state\"
-- part of @'CpuM'@.
data CpuState
  = CpuState
    { _cpuPC          :: PC
    , _cpuCarry       :: Bit
    , _cpuRegFile     :: CpuRegFile
    , _cpuDmem        :: CpuDataMem
    }

makeLenses ''CpuState

-- | Inputs to the AAP CPU, on every clock cycle. This is the \"reader\" part of
-- @'CpuM'@.
data CpuInput
  = CpuInput
    { _cpuInInstr :: Instr
    }

makeLenses ''CpuInput

-- | Default initial state of the AAP CPU.
defaultCpuState :: CpuState
defaultCpuState = CpuState
  defaultPC                      -- default program counter
  low                            -- no carry bit set
  (CpuRegFile (Nothing, Nothing) -- no register file read ports
              (\_ _ -> Nothing)) -- no register file write ports
  (CpuDataMem Nothing
              (\_ -> Nothing))

--------------------------------------------------------------------------------
-- CpuM Monad

-- | Monad for CPU execution. This is a internally a State/Reader monad that
-- gets implicitly unwrapped and turned into a transfer function, suitable for
-- using with @'CLaSH.Prelude.mealy'@ functions.
newtype CpuM a = CpuM (ReaderT CpuInput (State CpuState) a)
  deriving ( Functor, Applicative, Monad
           , MonadState  CpuState
           , MonadReader CpuInput
           )

-- | Execute a CPU action. This is typically done every cycle of the clock, with
-- the @'CpuState'@ being the state component of a mealy machine, and the
-- @'CpuInput'@ being represented as the input @'Signal'@s to the mealy machine,
-- for a particular clock cycle.
runCpuM :: CpuState      -- ^ Input CPU state
        -> CpuInput      -- ^ Inputs to the CPU on this clock cycle
        -> CpuM a        -- ^ Action to execute
        -> (a, CpuState) -- ^ Output value, and resulting state
runCpuM st inp (CpuM act) = runState (runReaderT act inp) st

--------------------------------------------------------------------------------
-- Conversion to a mealy transfer function

-- | A transfer function that can be used with @'CLaSH.Prelude.mealy'@. This
-- function is roughly equivalent to a @'CpuM'@ action under the hood, and any
-- @'CpuM'@ action
type TransferFunction o
  -- Input CPU state
  =  CpuState

  -- Inputs: transformed into the Reader parameter
  -> ( Instr -- Input: cpuInInstr, the current instruction
     )

  -- Outputs: new CPU state, and the return value of the mealy action
  -> ( CpuState, o )

-- | Transform a @'CpuM'@ action into a transfer function, suitable for use with
-- @'CLaSH.Prelude.mealy'@ functions.
cpuMealyT :: CpuM a             -- ^ Action to transform
          -> TransferFunction a -- ^ Resulting transfer function
cpuMealyT act st inp = swap (runCpuM st params act)
  where
    -- Construct the CPU input parameters out of the input tuple.
    params = CpuInput inp


{--

knowing that:

  swap :: (a, b) -> (b, a)
  runState :: State s o -> s -> (o, s)

then:

  runState                          :: State s o -> s -> (o, s)
    ==> { eta expansion }
  \a s -> runState a s              :: State s o -> s -> (o, s)
    ==> { add argument to 'a' }
  \a s i -> runState (a i) s        :: (i -> State s o) -> s -> i -> (o, s)
    ==> { swap result }
  \a s i -> swap (runState (a i) s) :: (i -> State s o) -> s -> i -> (s, o)

thus, given:

  go :: (i -> State s o) -> s -> i -> (s, o)
  go = \a s i -> swap (runState (a i) s)

we have:

  act :: i -> State s o

  go act :: s -> i -> (s, o)

which is the exact type of a mealy machine transfer function:

  mealy :: (s -> i -> (s, o)) -> s -> Signal i -> Signal o
  mealy (go act) :: Signal i -> Signal o

thus, any monadic action built on @'State'@, such as @'act'@, may be converted
to a mealy transfer function.

--}
