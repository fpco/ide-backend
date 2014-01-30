-- | Types for the messages to and fro the GHC server
--
-- It is important that none of the types here rely on the GHC library.
{-# LANGUAGE DeriveDataTypeable #-}
module IdeSession.GHC.API (
    -- * Requests
    module IdeSession.GHC.Requests
    -- * Responses
  , module IdeSession.GHC.Responses
    -- * Configuration
  , ideBackendApiVersion
  , hsExtensions
  , hsBootExtensions
  , cExtensions
  , cHeaderExtensions
  , sourceExtensions
  , cabalMacrosLocation
  ) where

import System.FilePath ((</>))

import IdeSession.GHC.Requests
import IdeSession.GHC.Responses

-- | For detecting runtime version mismatch between the server and the library
--
-- We use a Unix timestamp for this so that these API versions have some
-- semantics (http://www.epochconverter.com/, GMT).
ideBackendApiVersion :: Int
ideBackendApiVersion = 1391084140

{------------------------------------------------------------------------------
  Configuration
------------------------------------------------------------------------------}

-- | Haskell source files
hsExtensions :: [FilePath]
hsExtensions = [".hs", ".lhs"]

-- | Haskell @.boot@ files
hsBootExtensions :: [FilePath]
hsBootExtensions = [".hs-boot", ".lhs-boot"]

-- | C source files
cExtensions :: [FilePath]
cExtensions = [".c"]

-- | C header files
cHeaderExtensions :: [FilePath]
cHeaderExtensions = [".h"]

-- | Extensions of all source files we keep in our source directory.
sourceExtensions :: [FilePath]
sourceExtensions = hsExtensions
                ++ hsBootExtensions
                ++ cExtensions
                ++ cHeaderExtensions

-- TODO: perhaps create dist/build/autogen and put macros there so that
-- Cabal.autogenModulesDir can use it for compilation of C files?
cabalMacrosLocation :: FilePath -> FilePath
cabalMacrosLocation ideDistDir = ideDistDir </> "cabal_macros.h"
