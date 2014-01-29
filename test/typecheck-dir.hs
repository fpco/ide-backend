{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverlappingInstances #-}


{-# LANGUAGE OverloadedStrings #-}
import Data.Time
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as L
import System.IO (appendFile)
import Control.Monad (forM_)
import IdeSession hiding (defaultSessionConfig, defaultSessionInitParams)
import qualified IdeSession as IdeSession

import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad
import Control.DeepSeq (rnf)
import qualified Data.ByteString.Char8      as BSSC
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.UTF8  as BSL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, elemIndex)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, mempty, (<>))
import Data.Text (Text)
import Data.Char (isLower, isSpace)
import Data.Either (lefts)
import qualified Data.Text as Text
import Debug.Trace (traceEventIO)
import Data.Version (Version (..))
import Distribution.License (License (..))
import Distribution.Simple.Program.Find ( -- From our patched cabal
    ProgramSearchPath
  , findProgramOnSearchPath
  , ProgramSearchPathEntry(..)
  )
import Prelude hiding (mod, span)
import System.Directory
import qualified System.Environment as System.Environment
import System.Exit (ExitCode (..))
import System.FilePath
import System.FilePath.Find (always, extension, find)
import System.IO as IO
import System.IO.Temp (withTempDirectory, createTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
-- import System.Process (readProcess)
-- import qualified System.Process as Process
-- import System.Random (randomRIO)
import System.Timeout (timeout)
-- import Text.Regex (mkRegex, subRegex)



mkFile :: Int -> (FilePath, L.ByteString)
mkFile i =
    (name, contents)
  where
    name = "Module" ++ show i ++ ".hs"
    contents
        | i == 1 = L.pack $ unlines
            [ "module Module1 where"
            , "val1 = 1"
            ]
        | otherwise = L.pack $ unlines
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module Module" ++ show i ++ " where"
            , "import Module" ++ show (i - 1)
            , "import System.IO (appendFile)"
            , "import Language.Haskell.TH.Syntax"
            , "qRunIO (appendFile \"../log.txt\" \"TH: Compiling " ++ show i ++ "\\n\") >> return []"
            , "val" ++ show i ++ " = val" ++ show (i - 1)
            ]

say :: String -> IO ()
say msg = do
    now <- getCurrentTime
    appendFile "log.txt" $ show now ++ ": " ++ msg ++ "\n"
    print $ show now ++ ": " ++ msg ++ "\n"

main :: IO ()
main = do
    say "Entering main"
    session <- initSession (SessionInitParams Nothing) defaultSessionConfig {configDeleteTempFiles = False, configGenerateModInfo = False}
    say "Session initialized"

    forM_ [1..600] $ \i -> do
        let (name, contents) = mkFile i
        let update = updateSourceFile name contents
        say $ "applying " ++ show i
        updateSession session update $ \p -> say $ "progress == " ++ show p
        say $ "applied " ++ show i

defaultSessionInitParams :: SessionInitParams
{-# NOINLINE defaultSessionInitParams #-}
defaultSessionInitParams = unsafePerformIO $
  return IdeSession.defaultSessionInitParams

defaultSessionConfig :: SessionConfig
{-# NOINLINE defaultSessionConfig #-}
defaultSessionConfig = unsafePerformIO $ do
  packageDb     <- (System.Environment.getEnv "IDE_BACKEND_PACKAGE_DB")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  extraPathDirs <- (System.Environment.getEnv "IDE_BACKEND_EXTRA_PATH_DIRS")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  let packageDbStack
        | null packageDb = configPackageDBStack IdeSession.defaultSessionConfig
        | otherwise      = [GlobalPackageDB, SpecificPackageDB packageDb]
  return IdeSession.defaultSessionConfig {
             configPackageDBStack = packageDbStack
           , configExtraPathDirs  = splitSearchPath extraPathDirs
           }
