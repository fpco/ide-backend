{-# LANGUAGE TemplateHaskell #-}
-- | Valid Haskell qualified module names.
module ModuleName
  ( ModuleName(..)
  , toString
  , fromString
  , toFilePath
  , fromFilePath
  , LoadedModules
  ) where

import Data.Aeson.TH (deriveJSON)
import qualified Data.Char as Char (isAlphaNum, isUpper)
import Data.List (intercalate)
import System.FilePath (pathSeparator, splitDirectories)

-- | The type of qualified module names.
newtype ModuleName = ModuleName [String]
  deriving (Eq, Ord, Read, Show)

-- | The type of the list of qualified names of modules, as maintained
-- by the compiler.
type LoadedModules = [ModuleName]

-- | Pretty-print a qualified module name.
toString :: ModuleName -> String
toString (ModuleName ms) = intercalate "." ms

-- | Construct a 'ModuleName' from a valid module name 'String'.
-- It results in an error when used with a string that is not
-- a valid module name (wrong case of any name part, wrong symbols used).
--
fromString :: String -> Maybe ModuleName
fromString string
  | all validModuleComponent components' = Just $ ModuleName components'
  | otherwise                            = Nothing
 where
  components' = split string
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

-- | An auxiliary function for tests. Guesses a module name from a file path.
fromFilePath :: FilePath -> Maybe ModuleName
fromFilePath path = fromString $ toString $ ModuleName $
  case splitDirectories path of
    "." : l -> l
    "/" : l -> l
    l -> l

$(deriveJSON id ''ModuleName)
