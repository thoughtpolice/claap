{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Main
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Entry point for AAP simulation and test bench.
--
module Main
  ( main -- :: IO ()
  ) where
import Prelude

import Data.Maybe    ( fromMaybe )
import Control.Monad ( forM_ )
import System.Environment
import System.Console.GetOpt
import System.Exit

-- Synthesizable tests, run in simulation mode.
import AAP.Test     ( runSimTests )
-- Tests for the simulation tools
import AAP.Sim.Test ( runDisassemblerTests )

--------------------------------------------------------------------------------
-- Command line options

data TestFlag
  = SynthesizedTest String
  | SimulationTest  String

testOptions :: [OptDescr TestFlag]
testOptions =
  [ Option [] ["synthesized"] (OptArg synDefault "TEST")
      "simulate and run a test which can be synthesized (default: 'all')"
  , Option [] ["simulation"]  (OptArg simDefault "TEST")
      "run a simulation test which can't be synthesize (default: 'all')"
  ]
  where
    synDefault = SynthesizedTest . fromMaybe "all"
    simDefault = SimulationTest  . fromMaybe "all"

parseTestOptions :: [String] -> IO [TestFlag]
parseTestOptions argv = case getOpt Permute testOptions argv of
  (o, _, [])   -> return o
  (_, _, errs) -> do
    putStrLn $ concat errs ++ (usageInfo header testOptions)
    exitFailure
  where
    header = "Usage: simulate test [OPTION...]"

--------------------------------------------------------------------------------
-- Simulation driver: run the resulting /Haskell/ program.

-- | Main simulation driver.
main :: IO ()
main = getArgs >>= \case
  -- Bail if no arguments were given
  [] -> putStrLn "TODO FIXME" >> exitFailure

  -- Tests
  "test":argv -> do
    tests <- parseTestOptions argv
    forM_ tests $ \case
      SynthesizedTest t -> runSimTests t
      SimulationTest  _ -> runDisassemblerTests


  -- Bail in case of any invalid arguments
  _ -> putStrLn "TODO FIXME" >> exitFailure
