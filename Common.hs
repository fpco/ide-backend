{-# LANGUAGE TemplateHaskell #-}
-- | GHC-independent types that the GHC-specific modules nevertheless
-- need to know.
module Common
  ( SourceError(..), SourceErrorKind(..)
  , formatSourceError
  , SymbolDefinitionMap
  , hsExtentions
  , PCounter
  , RunResult(..)
  , showExWithClass
  , dVerbosity, debug
  , accessorName
  ) where

import qualified Control.Exception as Ex
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable (Typeable, typeOf)
import System.FilePath (takeFileName)
import System.IO (hFlush, stderr)

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

$(deriveJSON id ''SourceErrorKind)
$(deriveJSON id ''SourceError)

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

-- | This type represents intermediate progress information during compilation.
type PCounter = Int

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk String
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
  deriving (Show, Eq)

$(deriveJSON id ''RunResult)

-- | Show an exception together with its most precise type tag.
showExWithClass :: Ex.SomeException -> String
showExWithClass (Ex.SomeException ex) = show (typeOf ex) ++ ": " ++ show ex

dVerbosity :: Int
dVerbosity = 3

debugFile :: Maybe FilePath
debugFile = Nothing -- Just "debug.log"

debug :: MonadIO m => Int -> String -> m ()
debug verbosity msg =
  when (verbosity >= 3) $ do
    case debugFile of
      Nothing -> return ()
      Just logName ->
        liftIO $ appendFile logName $ msg ++ "\n"
    when (verbosity >= 4) $ liftIO $ do
      BS.hPutStrLn stderr $ BS.pack $ "debug: " ++ msg
      hFlush stderr

-- | Translate record field '_name' to the accessor 'name'
accessorName :: String -> Maybe String
accessorName ('_' : str) = Just str
accessorName _           = Nothing
