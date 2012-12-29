{-# LANGUAGE TemplateHaskell #-}
-- | Common types and utilities
module Common
  ( SourceError(..), SourceErrorKind(..)
  , formatSourceError
  , SymbolDefinitionMap
  , hsExtentions
  , Progress
  , initialProgress
  , updateProgress
  , progressStep
  , showExWithClass
  , dVerbosity, debug
  , accessorName
  , lookup'
  ) where

import qualified Control.Exception as Ex
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Typeable (Typeable, typeOf)
import System.FilePath (takeFileName)
import System.IO (hFlush, stderr)

import Data.Accessor (Accessor, accessor)

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data SourceError =
    SrcError SourceErrorKind FilePath (Int, Int) (Int, Int) String
  | OtherError String
  deriving (Show, Eq)

-- | Severity of an error.
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
newtype Progress = Progress Int
  deriving (Show, Eq, Ord)

initialProgress :: Progress
initialProgress = Progress 1  -- the progress indicates a start of a step

updateProgress :: String -> Progress -> Progress
updateProgress _msg (Progress n) = Progress (n + 1)

-- | The step number of the progress. Usually corresponds to the number
-- of files already processed.
progressStep :: Progress -> Int
progressStep (Progress n) = n

$(deriveJSON id ''Progress)

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

-- | Prelude.lookup as an accessor
lookup' :: Eq a => a -> Accessor [(a, b)] (Maybe b)
lookup' key =
    accessor (lookup key) $ \mVal list ->
      case mVal of
        Nothing  -> delete key list
        Just val -> override key val list
  where
    override :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
    override a b [] = [(a, b)]
    override a b ((a', b') : xs)
      | a == a'   = (a, b) : xs
      | otherwise = (a', b') : override a b xs

    delete :: Eq a => a -> [(a, b)] -> [(a, b)]
    delete _ [] = []
    delete a ((a', b') : xs)
      | a == a'   = xs
      | otherwise = (a', b') : delete a xs
