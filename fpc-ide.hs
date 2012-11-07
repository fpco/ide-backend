-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012

-- (JP Moresmau's buildwrapper package used as template for GHC API use)

{-# LANGUAGE CPP #-}

module Main where

import GHC hiding (flags, ModuleName)
import qualified Config as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif
import Outputable ( PprStyle, showSDocForUser, qualName, qualModule )
import FastString ( unpackFS )
import StringBuffer ( stringToStringBuffer )

import Text.JSON as JSON

import System.Environment
import System.Process
#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IORef
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Concurrent
import System.Directory
import System.Random (randomIO)
import System.FilePath (combine, takeExtension)
import System.IO.Unsafe (unsafePerformIO)  -- horrors

import Data.Monoid (Monoid(..))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import IdeSession

-- Test the stuff.

main :: IO ()
main = do
  args <- getArgs
  let configSourcesDir = case args of
        [dir] -> dir
        [] -> "."
        _ -> fail "usage: fpc-ide [source-dir]"
  let sessionConfig = SessionConfig{..}
  -- Two sample scenarios:
  b <- randomIO
  if b
    then do
      s0 <- initSession sessionConfig
      let update1 =
            (updateModule $ ModulePut "fpc-ide.hs" (BS.pack "1"))
            <> (updateModule $ ModulePut "fpc-ide.hs" (BS.pack "x = a1"))
          update2 =
            (updateModule $ ModulePut "fpc-ide.hs" (BS.pack "2"))
            <> (updateModule $ ModulePut "fpc-ide.hs" (BS.pack "x = a2"))
      progress1 <- updateSession s0 update1
      s2 <- progressWaitCompletion progress1
      msgs2 <- getSourceErrors s2
      putStrLn $ "Errors 2: " ++ List.intercalate "\n\n" msgs2
      progress3 <- updateSession s2 update2  -- s0 should fail
      s4 <- progressWaitCompletion progress3
      msgs4 <- getSourceErrors s4
      putStrLn $ "Errors 4: " ++ List.intercalate "\n\n" msgs4
      msgs2' <- getSourceErrors s2
      putStrLn $ "Errors 2 again: " ++ List.intercalate "\n\n" msgs2'
      shutdownSession s4
    else do
      s0 <- initSession sessionConfig
      progress <- updateSession s0 mempty
      s1 <- progressWaitCompletion progress
      msgs1 <- getSourceErrors s1
      putStrLn $ "Errors 1: " ++ List.intercalate "\n\n" msgs1
      shutdownSession s1  -- s0 should fail
