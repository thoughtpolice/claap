module Tools.ArachnePNR
  ( PcfFile
  , ArachneLogPath
  , BlifPath
  , arachne_pnr
  ) where

import Development.Shake

import Config

-- | Path to a pin constraint file.
type PcfFile = FilePath

-- | Path to the logs for place-and-route output.
type ArachneLogPath = FilePath

-- | Path to a BLIF-encoded netlist.
type BlifPath = FilePath

arachne_pnr :: PcfFile        -- ^ Input pcf
            -> ArachneLogPath -- ^ Output log file
            -> FilePath       -- ^ Output routed netlist
            -> BlifPath       -- ^ Input BLIF netlist
            -> Action ()
arachne_pnr pcf olog out inp = do
  arachne <- getArachne
  device  <- getDevice
  pkg     <- getPackage
  need [ inp, pcf ]

  let dopt = case device of
        HX1K -> "1k"
        LP1K -> "1k"
        HX8K -> "8k"
        LP8K -> "8k"
  let popt = case pkg of
        TQ144 -> "tq144"
        CT256 -> "ct256"

  let cmdOpts = [ EchoStderr False ]
  Stderr output <- cmd cmdOpts arachne
    [ "-d", dopt
    , "-P", popt
    , "-p", pcf
    , "-o", out
    , inp
    ]
  writeFile' olog output
