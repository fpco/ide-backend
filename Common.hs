{-# LANGUAGE TemplateHaskell #-}
-- | GHC-independent types that the GHC-specific modules nevertheless
-- need to know.
module Common
  ( SourceError(..), SourceErrorKind(..)
  , formatSourceError
  , SymbolDefinitionMap
  , hsExtentions, cpExtentions
  , PCounter
  , RunResult(..)
  , ModuleName(..)
  , LoadedModules
  , showExWithClass
  , dVerbosity, debug
  ) where

import Data.Aeson.TH (deriveJSON)
import System.FilePath (takeFileName)
import qualified Control.Exception as Ex
import Data.Typeable (Typeable, typeOf)
import Control.Monad (when)
import System.IO (hFlush, hPutStr, stderr)

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data SourceError =
    SrcError SourceErrorKind FilePath (Int, Int) (Int, Int) String
  | OtherError String
  deriving (Show, Eq)

data SourceErrorKind = KindError | KindWarning
  deriving (Show, Eq)

newtype ModuleName = ModuleName String
  deriving (Eq, Ord, Show)

$(deriveJSON id ''SourceErrorKind)
$(deriveJSON id ''SourceError)
$(deriveJSON id ''ModuleName)

formatSourceError :: SourceError -> String
formatSourceError (SrcError kind path ii jj s) =
  show (SrcError kind (takeFileName path) ii jj s)
formatSourceError err@(OtherError _ ) = show err

-- | A mapping from symbol uses to symbol definitions
--
-- * This is currently a stub, but it will be a full concrete type so that
-- it can be serialised etc.
--
data SymbolDefinitionMap

-- | These source files are type-checked.
hsExtentions:: [FilePath]
hsExtentions = [".hs", ".lhs"]

-- | These source files are either type-checked or used
-- for type-checking others, so they are worth copying over.
cpExtentions :: [FilePath]
cpExtentions = ".h" : hsExtentions

type PCounter = Int

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk String
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
  deriving Show

type LoadedModules = [ModuleName]

$(deriveJSON id ''RunResult)

-- | Show an exception together with its most precise type tag.
showExWithClass :: Ex.SomeException -> String
showExWithClass (Ex.SomeException ex) = show (typeOf ex) ++ ": " ++ show ex

dVerbosity :: Int
dVerbosity = 3

debugFile :: Maybe FilePath
debugFile = Nothing -- Just "debug.log"

debug :: Int -> String -> IO ()
debug verbosity msg =
  when (verbosity >= 3) $ do
    case debugFile of
      Nothing -> return ()
      Just logName ->
        appendFile logName $ msg ++ "\n"
    when (verbosity >= 4) $ do
      hPutStr stderr $ "debug: " ++ msg ++ "\n"
      hFlush stderr
