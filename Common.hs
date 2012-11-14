{-# LANGUAGE TemplateHaskell #-}
module Common
  ( SourceError(..), ErrorKind(..)
  , SymbolDefinitionMap
  ) where

import Data.Aeson.TH (deriveJSON)

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data SourceError =
    SrcError ErrorKind FilePath (Int, Int) (Int, Int) String
  | OtherError String
  deriving Show

data ErrorKind = Error | Warning
  deriving Show

$(deriveJSON id ''ErrorKind)
$(deriveJSON id ''SourceError)

-- | A mapping from symbol uses to symbol definitions
--
-- * This is currently a stub, but it will be a full concrete type so that
-- it can be serialised etc.
--
data SymbolDefinitionMap
