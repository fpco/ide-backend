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
ideBackendApiVersion = 1388408415

{------------------------------------------------------------------------------
  Configuration
------------------------------------------------------------------------------}

-- | These source files are type-checked.
hsExtensions :: [FilePath]
hsExtensions = [".hs", ".lhs"]
-- Boot files are not so simple. They should probably be copied to the src dir,
-- but not made proper targets. This is probably similar to .h files.
-- hsExtentions = [".hs", ".lhs", ".hs-boot", ".lhs-boot", ".hi-boot"]

-- | Extensions of files to compile using the C compiler.
cExtensions :: [FilePath]
cExtensions = [".c"]

-- | Extensions of C header files (for inclusion in .c and .hs files).
cHeaderExtensions :: [FilePath]
cHeaderExtensions = [".h"]

-- | Extensions of all source files we keep in our source directory.
sourceExtensions :: [FilePath]
sourceExtensions = cHeaderExtensions ++ cExtensions ++ hsExtensions

-- TODO: perhaps create dist/build/autogen and put macros there so that
-- Cabal.autogenModulesDir can use it for compilation of C files?
cabalMacrosLocation :: FilePath -> FilePath
cabalMacrosLocation ideDistDir = ideDistDir </> "cabal_macros.h"
