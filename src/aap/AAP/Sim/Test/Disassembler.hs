-- |
-- Module      : AAP.Sim.Test.Disassembler
-- Copyright   : (c) Austin Seipp 2016
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Test suite harness for the disassembler, when running CLaSH simulations.
--
module AAP.Sim.Test.Disassembler
  ( checkFile
  ) where
import CLaSH.Prelude
import qualified Prelude as P

import Data.Char       ( isSpace )
import Data.List       ( isPrefixOf )
import Data.List.Split ( splitOn )
import Text.Read       ( readMaybe )
import Control.Monad   ( guard, (>=>) )

import AAP.Decoder ( Instr(..) )
import AAP.Sim.Disassemble

type Tokens = [String]

-- | Tokenize a line like @add $r0, $r0, $r0@ into its constituent tokens,
-- @[\"add\", \"$r0\", \"$r0\", \"$r0\"]@.
tokenizeEncoding :: String -> Tokens
tokenizeEncoding = words . filter (/= ',')

-- | A disassembler tests consists of multiple @'CheckLine'@s, which specify a
-- set of bytes to disassemble, and the expected output tokens.
type CheckLine = ([Unsigned 8], Tokens)

-- | Strip comments and whitespace from a test file.
stripComments :: [String] -> [String]
stripComments
  = filter (not . ("#" `isPrefixOf`))
  . filter (P.not . P.null)
  . P.map (dropWhile isSpace)

-- | Parse a line of the disassembler tests. A line for the disassembler test
-- harness to check consists of a string of bytes and the assembler encoding,
-- like:
--
-- @[0x00,0x02] ;; encoding: add  $r0, $r0, $r0@
parseLine :: String -> Maybe CheckLine
parseLine inp = do
  let [b, t] = splitOn " ;; " inp

  bytes <- readMaybe b

  guard ("encoding: " `isPrefixOf` t)
  let tokens = tokenizeEncoding (P.drop 10 t)

  return (bytes, tokens)

parseDisasmFile :: FilePath -> IO [Maybe CheckLine]
parseDisasmFile inp = go <$> P.readFile inp where
  go = P.map parseLine
     . stripComments
     . P.lines

parseBytes :: [Unsigned 8] -> Maybe (Either (BitVector 16) (BitVector 32))
parseBytes [a, b]       = Just $ Left  (pack (a, b))
parseBytes [a, b, c, d] = Just $ Right (pack (a, b, c, d))
parseBytes _            = Nothing

checkFile :: FilePath -> IO ()
checkFile = parseDisasmFile >=> mapM_ k
  where
    k x = case x of
      Nothing ->
        putStrLn $ "ERROR: test had a parse error somewhere! "
              P.++ "(but I'm too stupid to remember where)"

      Just (bytes, tokens) -> do
        case parseBytes bytes of
          Nothing -> putStrLn $ "ERROR: Invalid number of bytes for disassembler test! (expected "
                           P.++ show tokens P.++ ")"
          Just op -> case either decode16 decode32 op of
            Invalid -> putStrLn $ "ERROR: I couldn't decode an invalid instruction! (expected "
                             P.++ show tokens P.++ ")"
            instr   -> do
              let tokenout = tokenizeEncoding (prettyInstr instr)
              case (tokenout == tokens) of
                True  -> return ()
                False -> putStrLn $ "ERROR: invalid disassembly (expected "
                               P.++ show tokens   P.++ ", got "
                               P.++ show tokenout P.++ ")"
