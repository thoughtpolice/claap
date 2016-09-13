{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds #-}
module Development.Shake.Fancy
    ( module Development.Shake

    , cmdWrap

    , shake
    , shakeArgs
    , Action
    , action
    , actionFinally
    , putNormal
    , putQuiet
    , phony
    , (%>)
    , (&%>)
    , (~>)
    , writeFile'
    , writeFileChanged
    , need
    , orderOnly
    , readFileLines
    , getDirectoryFiles
    , liftAction
    , doesFileExist
    , addOracle
    , newCache
    , alwaysRerun
    , readFile'
    , withTempFile
    , removeFilesAfter

    , S.usingConfigFile
    , getConfig
    )
    where

import Development.Shake hiding
    ( Action
    , shake
    , shakeArgs
    , action
    , actionFinally
    , putNormal
    , putQuiet
    , phony
    , (%>)
    , (&%>)
    , (~>)
    , writeFile'
    , writeFileChanged
    , getDirectoryFiles
    , need
    , orderOnly
    , readFileLines
    , doesFileExist
    , addOracle

    , newCache
    , alwaysRerun
    , readFile'
    , withTempFile
    , removeFilesAfter
    )
import qualified Development.Shake as S
--import qualified Development.Shake.Rule as S
import qualified Development.Shake.Classes as S
--import qualified Development.Shake.Command as S
import qualified Development.Shake.Config as S
import System.Console.Concurrent
import System.Console.Regions
import System.IO.Extra (newTempFile)

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
--import Control.Applicative
import Data.List

-- | Wrapper around 'S.shake'
shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = displayConsoleRegions $ S.shake opts rules

-- | Wrapper around 'S.shakeArgs'
shakeArgs :: ShakeOptions -> Rules () -> IO ()
shakeArgs opts rules = displayConsoleRegions $ S.shakeArgs opts rules

data FancyEnv = FancyEnv
    { currentTarget :: String
    , currentRegion :: ConsoleRegion
    }

-- | Wrapper around 'S.Action'
newtype Action a = Action (ReaderT FancyEnv S.Action a)

    deriving (Monad, Applicative, Functor, MonadIO)

runAction :: Action a -> FancyEnv -> S.Action a
runAction (Action fa) = runReaderT fa

mkAction :: (FancyEnv -> S.Action a) -> Action a
mkAction act = Action (ReaderT act)

liftAction :: S.Action a -> Action a
liftAction act = mkAction (const act)

finish :: FancyEnv -> IO ()
finish env = finishConsoleRegion (currentRegion env) $
    "✓ " ++ currentTarget env ++ " done"

wrapAction :: Action a -> String -> S.Action a
wrapAction act target = do
    region <- liftIO $ openConsoleRegion Linear
    let env = FancyEnv target region
    runAction act env `S.actionFinally` finish env

setDefaultMessage :: Action ()
setDefaultMessage = mkAction $ \env ->
   liftIO $ setConsoleRegion (currentRegion env) $
    "  " ++ currentTarget env ++ " processing..."

setMessage :: Char -> String -> Action ()
setMessage c doing = mkAction $ \env ->

   liftIO $ setConsoleRegion (currentRegion env) $
    [c] ++ " " ++ currentTarget env ++ " " ++ doing


describe :: S.Action a -> Char -> String -> Action a
describe act symb desc = do
    setMessage symb desc
    x <- liftAction act
    setDefaultMessage
    return x

-- | Wrapper around 'S.action'
action :: Action a -> Rules ()
action act = S.action $ wrapAction act "some action"

-- | Wrapper around 'S.actionFinally'
actionFinally :: Action a -> IO b -> Action a
actionFinally act io = mkAction $ \env -> runAction act env `S.actionFinally` io

-- | Wrapper around 'S.putNormal'
putNormal :: String -> Action ()
putNormal txt = mkAction $ \env -> do
    verb <- getVerbosity
    when (Normal >= verb) $ liftIO $ outputConcurrent $ currentTarget env ++ ": " ++ txt

putQuiet :: String -> Action ()
putQuiet txt = mkAction $ \env -> do
    verb <- getVerbosity
    when (Quiet >= verb) $ liftIO $ outputConcurrent $ currentTarget env ++ ": " ++ txt


phony :: String -> Action () -> Rules ()
phony str act = S.phony str $ wrapAction act str

-- | Wrapper around '%>'
(%>) :: FilePattern -> (FilePath -> Action ()) -> Rules () 
pat %> act = pat S.%> (\out -> wrapAction (act out) out)

(&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
pat &%> act = pat S.&%> (\out -> wrapAction (act out) (show out))

(~>) :: String -> Action () -> Rules ()
target ~> act = target S.~> wrapAction act target


-- | Wrapper around 'writeFile''
writeFile' :: FilePath -> String -> Action ()
writeFile' filepath content =
    describe (S.writeFile' filepath content) '→' ("writing " ++ filepath)

-- | Wrapper around 'writeFile''
writeFileChanged :: FilePath -> String -> Action ()
writeFileChanged filepath content =
    describe (S.writeFileChanged filepath content) '→' ("writing " ++ filepath)

readFileLines :: FilePath -> Action [String]
readFileLines filepath =
    describe (S.readFileLines filepath) '←' ("reading " ++ filepath)

doesFileExist :: FilePath -> Action Bool
doesFileExist filepath = liftAction $ S.doesFileExist filepath

removeFilesAfter :: FilePath -> [FilePattern] -> Action ()
removeFilesAfter path pat = liftAction $ S.removeFilesAfter path pat

withTempFile :: (FilePath -> Action a) -> Action a
withTempFile act = do
  (file, del) <- liftIO newTempFile
  act file `actionFinally` (liftIO del)

getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles path pattern =
  describe (S.getDirectoryFiles path pattern) '←' ("reading directory " ++ show pattern)

need :: [FilePath] -> Action ()
need filepaths =
    describe (S.need filepaths) '…' ("waiting for " ++ take 60 (intercalate ", " filepaths))

readFile' :: FilePath -> Action String
readFile' x = need [x] >> liftIO (readFile x)

orderOnly :: [FilePath] -> Action ()
orderOnly filepaths =
    describe (S.orderOnly filepaths) '…' ("waiting for " ++ take 60 (intercalate ", " filepaths))

alwaysRerun :: Action ()
alwaysRerun = liftAction S.alwaysRerun

getConfig :: String -> Action (Maybe String)
getConfig = liftAction . S.getConfig

cmdWrap :: String -> S.Action a -> Action a
cmdWrap cmd' act =
    describe (quietly act) '!' ("running " ++ cmd')

addOracle :: (S.ShakeValue q, S.ShakeValue a) => (q -> Action a) -> S.Rules (q -> Action a)
addOracle action' = do
    query <- S.addOracle (\q -> wrapAction (action' q) (show q))
    return $ liftAction . query

newCache :: (Eq k, S.Hashable k) => (k -> Action v) -> Rules (k -> Action v)
newCache cache = do
    query <- S.newCache (\k -> wrapAction (cache k) "cache")
    return $ liftAction . query
