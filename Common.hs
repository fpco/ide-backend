{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | GHC-independent types that the GHC-specific modules nevertheless
-- need to know.
module Common
  ( SourceError(..), SourceErrorKind(..)
  , formatSourceError
  , SymbolDefinitionMap
  , hsExtentions, cpExtentions
  , ModuleName(..), internalFile
  , PCounter
  , RunResult, RunException, RunOutcome
  , showExWithClass
  , dVerbosity, debug
  ) where

import Data.Aeson.TH (deriveJSON)
import qualified Control.Exception as Ex
import Data.Typeable (Typeable, typeOf)
import Data.Maybe (catMaybes)
import Control.Monad (when)
import System.IO (hFlush, hPutStr, stderr)
import System.FilePath ((</>), (<.>), takeExtension, takeFileName)
#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data SourceError =
    SrcError SourceErrorKind FilePath (Int, Int) (Int, Int) String
  | OtherError String
  deriving Show

data SourceErrorKind = KindError | KindWarning
  deriving Show

newtype ModuleName = ModuleName String
  deriving Show

$(deriveJSON id ''SourceErrorKind)
$(deriveJSON id ''SourceError)
$(deriveJSON id ''ModuleName)
#if __GLASGOW_HASKELL__ >= 706
$(deriveJSON id ''UTCTime)
#else
$(deriveJSON id ''ClockTime)
#endif

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

internalFile :: FilePath -> ModuleName -> FilePath
internalFile configSourcesDir (ModuleName n) =
  let ext = takeExtension n
  in if ext `elem` cpExtentions
     then configSourcesDir </> n            -- assume full file name
     else configSourcesDir </> n <.> ".hs"  -- assume bare module name

type PCounter = Int

type RunResult = String
type RunException = String

-- | The first component holds compilation warnings and errors
-- (including arbitrary compilation exceptions raised by the compiler API).
-- The second component, empty if no code execution was requested,
-- holds an identifier bound to the resulting value or a (pretty-printed)
-- exception thrown by the user code.
type RunOutcome = ([SourceError], Maybe (Either RunResult RunException))

-- | Show an exception together with its most precise type tag.
-- All exception classes defined in Control.Exception are handled
-- (there are a few more instances of Exception defined elsewhere).
showExWithClass :: Ex.SomeException -> String
showExWithClass ex =
  let fr :: Ex.Exception e => Ex.SomeException -> Maybe e
      fr = Ex.fromException
      fshow :: (Show e, Typeable e) => Maybe e -> Maybe String
      fshow = fmap $ \ e -> (show (typeOf e) ++ ": " ++ show e)
      exs = catMaybes $
        [ fshow (fr ex :: Maybe Ex.IOException)
        , fshow (fr ex :: Maybe Ex.ErrorCall)
        , fshow (fr ex :: Maybe Ex.ArithException)
        , fshow (fr ex :: Maybe Ex.ArrayException)
        , fshow (fr ex :: Maybe Ex.AssertionFailed)
        , fshow (fr ex :: Maybe Ex.AsyncException)
        , fshow (fr ex :: Maybe Ex.NonTermination)
        , fshow (fr ex :: Maybe Ex.NestedAtomically)
        , fshow (fr ex :: Maybe Ex.BlockedIndefinitelyOnMVar)
        , fshow (fr ex :: Maybe Ex.BlockedIndefinitelyOnSTM)
        , fshow (fr ex :: Maybe Ex.Deadlock)
        , fshow (fr ex :: Maybe Ex.NoMethodError)
        , fshow (fr ex :: Maybe Ex.PatternMatchFail)
        , fshow (fr ex :: Maybe Ex.RecUpdError)
        , fshow (fr ex :: Maybe Ex.RecConError)
        , fshow (fr ex :: Maybe Ex.RecSelError)
        , -- This one is always not Nothing.
          fshow (fr ex :: Maybe Ex.SomeException)
        ]
  in head exs

dVerbosity :: Int
dVerbosity = 4

debugFile :: Maybe FilePath
debugFile = Just "debug.log"

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
