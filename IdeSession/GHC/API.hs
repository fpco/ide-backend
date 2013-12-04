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
  , cHeaderExtensions
  , sourceExtensions
  , cabalMacrosLocation
    -- * Warnings
  , GhcWarnings(..)
  , defaultGhcWarnings
  , ghcWarningsString
  , stringGhcWarnings
  ) where

import System.FilePath ((</>))

import IdeSession.GHC.Requests
import IdeSession.GHC.Responses

-- | For detecting runtime version mismatch between the server and the library
--
-- We use a Unix timestamp for this so that these API versions have some
-- semantics (http://www.epochconverter.com/, GMT).
ideBackendApiVersion :: Int
ideBackendApiVersion = 1386165289

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

cabalMacrosLocation :: FilePath -> FilePath
cabalMacrosLocation ideDistDir = ideDistDir </> "cabal_macros.h"

{------------------------------------------------------------------------------
  Warnings
------------------------------------------------------------------------------}

-- | Enable/disable warnings. We introduce this separate from configStaticOpts
-- because some warnings are introduced only in later versions of GHC; setting
-- or unsetting these warnings in earlier GHC versions simply has no effect
-- (whilst specifying the corresponding option in configStaticOpts would
-- result in an unrecognized flag error).
--
-- A "Nothing" value leaves the default.
data GhcWarnings = GhcWarnings {
    -- | AMP warning (GHC >= 7.8)
    --
    -- <http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal>
    ghcWarningAMP :: Maybe Bool

    -- | Deprecated flags
  , ghcWarningDeprecatedFlags :: Maybe Bool
  }

-- | Leave all warnings at their ghc-default
defaultGhcWarnings :: GhcWarnings
defaultGhcWarnings = GhcWarnings {
    ghcWarningAMP             = Nothing
  , ghcWarningDeprecatedFlags = Nothing
  }

-- | We use this when we want to transmit a GhcWarnings to the ghc server;
-- we pass the result string on the command line to the server executable
ghcWarningsString :: GhcWarnings -> String
ghcWarningsString (GhcWarnings warningAMP
                               warningDeprecatedFlags) = [
      enc warningAMP
    , enc warningDeprecatedFlags
    ]
  where
    enc :: Maybe Bool -> Char
    enc Nothing      = 'D'
    enc (Just True)  = 'Y'
    enc (Just False) = 'N'

-- | Inverse of 'ghcWarningsString'
stringGhcWarnings :: String -> GhcWarnings
stringGhcWarnings [ warningAMP
                  , warningDeprecatedFlags
                  ] = GhcWarnings {
      ghcWarningAMP             = dec warningAMP
    , ghcWarningDeprecatedFlags = dec warningDeprecatedFlags
    }
  where
    dec 'D' = Nothing
    dec 'Y' = Just True
    dec 'N' = Just False
    dec _   = error "stringGhcWarnings: invalid string"
stringGhcWarnings _ = error "stringGhcWarnings: invalid string (wrong length)"
