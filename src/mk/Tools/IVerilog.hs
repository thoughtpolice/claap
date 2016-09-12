module Tools.IVerilog
  ( iverilog
  , vvp
  ) where

import Development.Shake

import Config

iverilog :: FilePath
         -> [FilePath]
         -> Action ()
iverilog out srcs = do
  exe <- getIVerilog

  need srcs
  cmd exe ["-o", out] srcs

vvp :: FilePath
    -> [String]
    -> Action ()
vvp inp args = do
  exe <- getVVP

  need [ inp ]
  cmd exe args [inp]
