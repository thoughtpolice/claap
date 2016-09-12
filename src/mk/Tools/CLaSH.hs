{-# LANGUAGE RecordWildCards #-}

module Tools.CLaSH
  ( ClashMode(..)
  , ClashOpts(..)
  , clash
  ) where

import Development.Shake

import Config

data ClashMode
  = Simulation FilePath
  | Verilog FilePath
  | SystemVerilog FilePath
  | VHDL FilePath

data ClashOpts
  = ClashOpts
    { clashMainIs :: Maybe String
    , clashObjdir :: FilePath
    , clashMode   :: ClashMode
    , clashIncdir :: [FilePath]
    }

clash :: ClashOpts  -- ^ Clash options
      -> [FilePath] -- ^ Input files
      -> Action ()
clash ClashOpts{..} inp = do
  clashExe <- getClash
  need inp

  let opts = case clashMode of
        Simulation x    -> [ "-O2", "-o", x ]
        Verilog x       -> [ "--verilog", "-clash-hdldir", x ]
        SystemVerilog x -> [ "--systemverilog", "-clash-hdldir", x ]
        VHDL x          -> [ "--vhdl", "-clash-hdldir", x ]

  cmd [ EchoStdout False, EchoStderr False ] clashExe "-v0"
    (  maybe [] (\x -> ["-main-is", x]) clashMainIs
    ++ [ "-hidir", clashObjdir ]
    ++ [ "-odir",  clashObjdir ]
    ++ opts
    ++ map ("-i"++) clashIncdir
    ++ inp
    )
