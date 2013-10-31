-- | Types for the messages to and fro the GHC server
--
-- It is important that none of the types here rely on the GHC library.
module IdeSession.GHC.API (
    -- * Requests
    module IdeSession.GHC.Requests
    -- * Responses
  , module IdeSession.GHC.Responses
    -- * Configuration
  , ideBackendApiVersion
  , hsExtensions
  , cExtensions
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
ideBackendApiVersion = 1382516208

{------------------------------------------------------------------------------
  Configuration
------------------------------------------------------------------------------}

-- | These source files are type-checked.
hsExtensions :: [FilePath]
hsExtensions = [".hs", ".lhs"]

-- | Extensions of files to compile using the C compiler.
cExtensions :: [FilePath]
cExtensions = [".c"]

-- | Extensions of all source files we keep in our source directory.
sourceExtensions :: [FilePath]
sourceExtensions = [".hs-boot", ".lhs-boot", ".hi-boot"]
                   ++ [".h"] ++ cExtensions
                   ++ hsExtensions

cabalMacrosLocation :: FilePath -> FilePath
cabalMacrosLocation ideSourcesDir = ideSourcesDir </> "cabal_macros.h"
