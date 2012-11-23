{-# LANGUAGE TemplateHaskell #-}
-- | GHC-independent types that the GHC-specific modules nevertheless
-- need to know.
module Common
  ( SourceError(..), SourceErrorKind(..)
  , formatSourceError
  , SymbolDefinitionMap
  , hsExtentions, cpExtentions
  ) where

import Data.Aeson.TH (deriveJSON)
import System.FilePath (takeFileName)

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

-- | These source files are either type-checked or used
-- for type-checking others, so they are worth copying over.
cpExtentions :: [FilePath]
cpExtentions = ".h" : hsExtentions
