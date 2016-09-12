module Tools.ArachnePNR
  ( arachne_pnr
  ) where

import Development.Shake

import Config

arachne_pnr :: FilePath     -- ^ Input pin file
            -> FilePath     -- ^ Output log file
            -> FilePath     -- ^ Output routed netlist
            -> FilePath     -- ^ Input BLIF netlist
            -> Action ()
arachne_pnr pins olog out inp = do
  arachne <- getArachne
  device  <- getDevice
  pkg     <- getPackage
  need [ inp, pins ]

  let dopt = case device of
        HX1K -> "1k"
        LP1K -> "1k"
        HX8K -> "8k"
        LP8K -> "8k"
  let popt = case pkg of
        TQ144 -> "tq144"
        CT256 -> "ct256"

  Stderr output <- cmd [ EchoStderr False ] [arachne]
    [ "-d", dopt
    , "-P", popt
    , "-p", pins
    , "-o", out
    , inp
    ]
  writeFile' olog output
