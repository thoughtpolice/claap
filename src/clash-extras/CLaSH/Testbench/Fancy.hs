{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

-- |
-- Module      : CLaSH.Testbench.Fancy
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Fancy test interface for running test benches in simulation mode, with a
-- nicer API (to integrate into e.g. your own test harness).
--
-- __NB__: The functions in this module are not synthesizable.
--
module CLaSH.Testbench.Fancy
  ( SimulatedTestResult(..) -- :: *
  , simulatedTestResults    -- ::

  , simulationTest          -- :: ...
  ) where
import Prelude               hiding ((!!), length)

import GHC.Generics          (Generic)
import GHC.TypeLits          (KnownNat)
import Control.DeepSeq       (NFData)

import CLaSH.Signal          (Signal, fromList, sampleN)
import CLaSH.Signal.Explicit (Signal', SClock, register', systemClock)
import CLaSH.Signal.Bundle   (unbundle)
import CLaSH.Sized.Index     (Index)
import CLaSH.Sized.Vector    (Vec, (!!), length)

import CLaSH.Promoted.Nat    (SNat(..), snatToInteger)

-- | Result of an individual test (one of many inside the test bench).
data SimulatedTestResult
  = TestOK            -- ^ Test passed
  | TestFailed String -- ^ Test failed
  | TestsDone         -- ^ No more tests. You shouldn't ever see this
                      -- constructor yourself.
  deriving (Eq, Show, Generic, NFData)

-- | Get some result information from a set of simulated tests: the number of
-- passed tests, and a list of any errors for any tests that failed.
simulatedTestResults :: [SimulatedTestResult]
                     -- ^ A list of test results.
                     -> (Int, [String])
                     -- ^ The number of passed tests, and a list of error
                     -- messages for any tests that failed.
simulatedTestResults = foldr go (0, []) where
  go TestOK           (!n, xs) = (n+1, xs)
  go (TestFailed err) (!n, xs) = (n,   err:xs)
  go TestsDone        (!n, xs) = (n,   xs) -- shouldn't ever happen

-- | Cousin function to @'CLaSH.Prelude.Testbench.outputVerifier'@, but instead
-- of returning @'Bool'@ results, it returns a @'SimulatedTestResult'@ signal.
simulatedOutputVerifier :: forall l a . (KnownNat l, Eq a, Show a)
                        => Vec l a
                        -- ^ Samples to compare with
                        -> Signal a
                        -- ^ Signal to verify
                        -> Signal SimulatedTestResult
                        -- ^ Output signal indicating test result
simulatedOutputVerifier = simulatedOutputVerifier' systemClock

-- | A version of @'simulatedOutputVerifier'@ that can be synchronized to
-- an arbitrary clock.
simulatedOutputVerifier' :: forall l clk a . (KnownNat l, Eq a, Show a)
                => SClock clk
                -- ^ Clock to which the input signal is synchronized to
                -> Vec l a
                -- ^ Samples to compare with
                -> Signal' clk a
                -- ^ Signal to verify
                -> Signal' clk SimulatedTestResult
                -- ^ Indicator that all samples are verified
simulatedOutputVerifier' clk samples i =
    let (s,o) = unbundle (genT <$> register' clk 0 s)
        (e,f) = unbundle o
    in simulatedAssert' clk i e (register' clk TestOK f)
  where
    genT :: Index l -> (Index l,(a, SimulatedTestResult))
    genT s = (s',(samples !! s,finished))
      where
        maxI = toEnum (length samples - 1)

        s' = if s < maxI
                then s + 1
                else s

        finished = if s == maxI then TestsDone else TestOK

-- | An assertion function for simulation tests.
simulatedAssert' :: (Eq a,Show a)
        => SClock t
        -> Signal' t a -- ^ Checked value
        -> Signal' t a -- ^ Expected value
        -> Signal' t SimulatedTestResult
        -> Signal' t SimulatedTestResult
simulatedAssert' clk checked expected returned =
  (\c e cnt r ->
      if c == e
       then r
       else TestFailed (concat [ "cycle(" ++ show clk ++ "): "
                               , show cnt
                               , ", "
                               , "expected: "
                               , show e
                               , ", got: "
                               , show c
                               ]))
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned

-- | Run a test in simulation mode, and give a friendly report of the results.
--
-- __NB__: This function CANNOT be synthesized; it is merely a more
-- user-friendly test interface, meant so you can easily run your tests inside
-- of CLaSH. To create test benches which can be synthesized, see
-- @'CLaSH.Prelude.Testbench'@.
simulationTest :: forall a n. (Eq a, Show a, KnownNat n)
               => Vec n a  -- ^ Expected list of outputs from a given signal.
               -> Signal a -- ^ Input signal
               -> [SimulatedTestResult]
simulationTest out inp = sampleN n (simulatedOutputVerifier out inp)
  where n = fromIntegral (snatToInteger (SNat :: SNat n))
