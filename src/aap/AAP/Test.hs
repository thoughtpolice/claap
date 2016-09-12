-- |
-- Module      : AAP.Test
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Entry point for all AAP synthesizable tests, when run in simulation mode.
--
module AAP.Test
  ( runSimTests -- :: String -> IO ()
  ) where
import Prelude

import Data.Maybe            ( fromMaybe )
import Control.Monad         ( forM_ )
import System.Exit           ( exitFailure )
import Text.Printf           ( printf )

import CLaSH.Testbench.Fancy ( SimulatedTestResult
                             , simulatedTestResults )

import qualified AAP.Test.Decoder as Decoder

--------------------------------------------------------------------------------
-- Main harness to run all the different simulation tests

runSimTests :: String -> IO ()
runSimTests t = fromMaybe nope (lookup t tests)
  where
    nope  = printf "Invalid simulation test: %s\n" t >> exitFailure

    tests = [ ("all",     mapM_ runSimTests (map fst $ tail tests))
            , ("decoder", report Decoder.simulationResults)
            ]

--------------------------------------------------------------------------------
-- Utilities

report :: [SimulatedTestResult] -> IO ()
report inp = case failed of
  []   -> printf "OK - passed %d tests\n" passed
  errs -> do
    printf "ERROR - %d tests failed! Results:\n" (length errs)
    forM_ (zip [0..] errs) $ \(i, err) ->
      printf "\tERROR (test #%d): %s\n" (i :: Integer) err
    -- sad trombone
    exitFailure
  where
    (passed, failed) = simulatedTestResults inp
