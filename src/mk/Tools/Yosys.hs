module Tools.Yosys
  ( YosysSynthType(..)
  , YosysOutput(..)
  , YosysConfig(..)

  , yosysConfig
  , yosys
  ) where

import Development.Shake

import Config

data YosysSynthType
  = Generic
  | Prep
  | Xilinx
  | ICE40
  | GreenPAK4

data YosysOutput
  = BLIF FilePath
  | EDIF FilePath
  | NoNetlist

data YosysConfig
  = YosysConfig
    { yosysTopModule    :: String
    , yosysLog          :: Maybe FilePath
    , yosysSynth        :: YosysSynthType
    , yosysSynthVerilog :: Maybe FilePath
    , yosysVerbosity    :: Maybe Int
    , yosysOutput       :: YosysOutput
    }

yosysConfig :: YosysConfig
yosysConfig = YosysConfig
  { yosysTopModule    = ""
  , yosysLog          = Nothing
  , yosysSynth        = Generic
  , yosysSynthVerilog = Nothing
  , yosysVerbosity    = Just 3
  , yosysOutput       = BLIF ""
  }

yosys :: YosysConfig -- ^ Yosys configuration options
      -> [FilePath]  -- ^ Input Verilog files
      -> Action ()
yosys config srcs =
  withTempFile $ \scriptFile -> do
    writeFile' scriptFile (unlines script)

    yo <- getYosys
    need srcs
    cmd [ EchoStdout False, EchoStderr False ]
      [yo] verbosity logOpts ["-s", scriptFile]

  where
    verbosity  = maybe [] (\x -> ["-v"++show x]) (yosysVerbosity config)
    logOpts    = maybe [] (\x -> ["-l", x])      (yosysLog config)

    top        = yosysTopModule config
    synthRule  = case yosysSynth config of
      Generic   -> "synth -top " ++ top
      Prep      -> "prep -top " ++ top
      Xilinx    -> "synth_xilinx -top " ++ top
      ICE40     -> "synth_ice40 -top " ++ top
      GreenPAK4 -> "synth_greenpak4 -top " ++ top

    netlistOpts = case yosysOutput config of
      NoNetlist -> ""
      BLIF f    -> "-blif " ++ f
      EDIF f    -> "-edif " ++ f

    maybeVerilog = maybe [] k (yosysSynthVerilog config) where
      k x = [ "write_verilog -attr2comment " ++ x ]

    script =
      -- Read every Verilog file
      map ("read_verilog " ++) srcs
      -- Write out the desired netlist
      ++ [ synthRule ++ " " ++ netlistOpts ]
      -- Synthesize verilog, if desired
      ++ maybeVerilog
