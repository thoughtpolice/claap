module Tools.Icestorm
  ( icepack
  , icetime
  , iceprog
  ) where

import Data.List  (isPrefixOf)
import Text.Read

import Development.Shake

import Config

icepack :: FilePath  -- ^ Output file
        -> FilePath  -- ^ Input file
        -> Action ()
icepack out inp = do
  exe <- getIcepack
  need [ inp ]

  cmd exe [inp] [out]

icetime :: Bool            -- ^ Should the @icetime@ command be Traced?
        -> Maybe Int       -- ^ Optional timing constraint check (in Mhz)
        -> Maybe FilePath  -- ^ Optional output log file
        -> FilePath        -- ^ Input file
        -> Action (Double, Double) -- ^ Resulting path delay and top Mhz
icetime traceCmd check out inp = do
  ty <- getDevice
  exe <- getIcetime
  need [ inp ]

  let devty = case ty of
        HX1K -> "hx1k"
        LP1K -> "lp1k"
        HX8K -> "hx8k"
        LP8K -> "lp8k"
  let checkopt = case check of
        Nothing -> []
        Just x  -> [ "-c", show x ]
  let opts = checkopt ++ [ "-tm", "-d", devty, inp ]

  let cmdOpts =
        -- Trace the output of the command if asked
        if not traceCmd then [ Traced "" ] else []
        -- Also, if icetime fails, then include Stdout
        -- in the error message.
        ++ [ WithStdout True ]

  Stdout output <- cmd cmdOpts exe opts
  -- Optionally write the output
  maybe (return ()) (flip writeFile' output) out

  -- Finally, parse the results of 'output' and get the total path delay, and
  -- the corresponding maximum clock frequency that's possible for the circuit

  -- Find the right line
  let delayLine = filter ("Total path delay: " `isPrefixOf`) (lines output)

  -- Do the appropriate dance to parse the output from IceTime
  case delayLine of
    -- Couldn't find the line?
    [] -> do
      putQuiet "WARNING: Couldn't parse icetime report (no line)!"
      return (0, 0)

    -- Uh, somehow we found *too many* lines?
    x | length x /= 1 -> do
      putQuiet "WARNING: Couldn't parse icetime report (too many lines)!"
      return (0, 0)

    -- OK, we found what we expected
    [x] -> do
      -- Drop the first part of the line
      let ws = drop 3 (words x)

      -- parse the results
      let pathDelay = readMaybe (head ws)
          maxFreq   = readMaybe (drop 1 (head (drop 2 ws)))
          res       = (,) <$> pathDelay <*> maxFreq

      case res of
        Just r  -> return r
        Nothing -> do
          putQuiet $ show (pathDelay, maxFreq)
          putQuiet "WARNING: Couldn't parse icetime report!"
          return (0, 0)

    -- Suppress warnings
    _ -> error "Tools.Icestorm.icetime: impossible!"

iceprog :: FilePath
        -> Action ()
iceprog inp = do
  mode <- getProgMode
  exe <- getIceprog
  need [ inp ]

  let opt = case mode of
        SRAM -> "-S"
        SPI  -> ""
  cmd [ EchoStderr False ] exe [opt] [inp]
