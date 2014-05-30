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
    -- * Paths
  , ideSessionSourceDir
  , ideSessionDataDir
  , ideSessionDistDir
  , ideSessionObjDir
  ) where

import System.FilePath ((</>))

import IdeSession.GHC.Requests
import IdeSession.GHC.Responses

-- | For detecting runtime version mismatch between the server and the library
--
-- We use a Unix timestamp for this so that these API versions have some
-- semantics (http://www.epochconverter.com/, GMT).
ideBackendApiVersion :: Int
ideBackendApiVersion = 1401451222

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

{-------------------------------------------------------------------------------
  Paths

  These are all meant to be relative to the session dir
-------------------------------------------------------------------------------}

-- | The directory to use for managing source files.
ideSessionSourceDir :: FilePath -> FilePath
ideSessionSourceDir sessionDir = sessionDir </> "src"

-- | The directory to use for data files that may be accessed by the
-- running program. The running program will have this as its CWD.
ideSessionDataDir :: FilePath -> FilePath
ideSessionDataDir sessionDir = sessionDir </> "data"

-- | Cabal "dist" prefix.
ideSessionDistDir :: FilePath -> FilePath
ideSessionDistDir sessionDir = sessionDir </> "dist"

-- | Directory where we store compiled C files (objects)
ideSessionObjDir :: FilePath -> FilePath
ideSessionObjDir sessionDir = sessionDir </> "ffi"
