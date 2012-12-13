{-# LANGUAGE TemplateHaskell #-}
-- | Valid Haskell module names.
module ModuleName
  ( ModuleName(..)
  , toString
  , fromString
  , toFilePath
  , LoadedModules
  ) where

import qualified Data.Char as Char (isAlphaNum, isUpper)
import System.FilePath (pathSeparator)
import Data.List (intercalate)
import Data.Aeson.TH (deriveJSON)

newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord, Read, Show)

type LoadedModules = [ModuleName]

toString :: ModuleName -> String
toString (ModuleName ms) = intercalate "." ms

-- | Construct a 'ModuleName' from a valid module name 'String'.
--
-- This is just a convenience function intended for valid module strings. It is
-- an error if it is used with a string that is not a valid module name. If you
-- are parsing user input then use 'Distribution.Text.simpleParse' instead.
--
fromString :: String -> ModuleName
fromString string
  | all validModuleComponent components' = ModuleName components'
  | otherwise                            = error badName

  where
    components' = split string
    badName     = "ModuleName.fromString: invalid module name " ++ show string

    split cs = case break (=='.') cs of
      (chunk,[])     -> chunk : []
      (chunk,_:rest) -> chunk : split rest

validModuleChar :: Char -> Bool
validModuleChar c = Char.isAlphaNum c || c == '_' || c == '\''

validModuleComponent :: String -> Bool
validModuleComponent []     = False
validModuleComponent (c:cs) = Char.isUpper c
                           && all validModuleChar cs

-- | Convert a module name to a file path, but without any file extension.
-- For example:
--
-- > toFilePath (fromString "A.B.C") = "A/B/C"
--
toFilePath :: ModuleName -> FilePath
toFilePath (ModuleName ms) = intercalate [pathSeparator] ms

$(deriveJSON id ''ModuleName)
