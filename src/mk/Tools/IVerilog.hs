module Tools.IVerilog
  ( iverilog
  , vvp
  ) where

import Development.Shake.Fancy

import Config

iverilog :: FilePath
         -> [FilePath]
         -> Action ()
iverilog out srcs = do
  exe <- getIVerilog

  need srcs
  cmdWrap exe $ cmd [exe] ["-o", out] srcs

vvp :: FilePath
    -> [String]
    -> Action ()
vvp inp args = do
  exe <- getVVP

  need [ inp ]
  cmdWrap exe $ cmd [exe] args [inp]
