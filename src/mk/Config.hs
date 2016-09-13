module Config
  ( ICE40Device(..)
  , ICE40Package(..)
  , ProgrammingMode(..)

  , getYosys, getArachne
  , getIcepack, getIcetime, getIceprog
  , getClash
  , getIVerilog, getVVP
  , getCheckMhz
  
  , getProgMode
  , getDevice
  , getPackage

  , getICE40PCF
  ) where

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Typeable
import Control.Exception
import Control.Monad.IO.Class

import Development.Shake.Fancy

data ICE40Device
  = HX1K  | HX8K
  | LP1K  | LP8K
  deriving (Eq, Show, Read)

data ICE40Package = TQ144 | CT256
  deriving (Eq, Show, Read)

data ProgrammingMode = SRAM | SPI
  deriving (Eq, Show, Read)

data InvalidConfigException = InvalidConfigException String
  deriving (Show, Typeable)

instance Exception InvalidConfigException

getYosys, getArachne   :: Action FilePath
getIcepack, getIcetime :: Action FilePath
getIceprog             :: Action FilePath
getClash               :: Action FilePath
getIVerilog, getVVP    :: Action FilePath

getCheckMhz            :: Action (Maybe Int)

getProgMode            :: Action ProgrammingMode
getDevice              :: Action ICE40Device
getPackage             :: Action ICE40Package
getICE40PCF            :: Action FilePath

getYosys    = fromMaybe "yosys"       <$> getConfig "YOSYS"
getArachne  = fromMaybe "arachne-pnr" <$> getConfig "ARACHNE_PNR"
getIcepack  = fromMaybe "icepack"     <$> getConfig "ICEPACK"
getIcetime  = fromMaybe "icetime"     <$> getConfig "ICETIME"
getIceprog  = fromMaybe "iceprog"     <$> getConfig "ICEPROG"
getClash    = fromMaybe "clash"       <$> getConfig "CLASH"
getIVerilog = fromMaybe "iverilog"    <$> getConfig "IVERILOG"
getVVP      = fromMaybe "vvp"         <$> getConfig "VVP"

getCheckMhz = do
  cfg <- getConfig "ICETIME_CHECK_MHZ"
  return (cfg >>= readMaybe)

getProgMode = do
  m <- fromMaybe "SRAM" <$> getConfig "PMODE"
  let err = badConfig $ "Invalid PMODE setting: " ++ m
  maybe err return (readMaybe m)

getDevice = do
  m <- fromMaybe "HX8K" <$> getConfig "ICE40_DEVICE"
  let err = badConfig $ "Invalid ICE40_DEVICE setting: " ++ m
  maybe err return (readMaybe m)

getPackage = do
  m <- fromMaybe "CT256" <$> getConfig "ICE40_PKG"
  let err = badConfig $ "Invalid ICE40_PKG setting: " ++ m
  maybe err return (readMaybe m)

getICE40PCF = fromMaybe "hx8k-breakout-board.pcf" <$> getConfig "ICE40_PCF"

badConfig :: String -> Action a
badConfig = liftIO . throwIO . InvalidConfigException
