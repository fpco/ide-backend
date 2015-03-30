-- | Abstract over differences in ghc versions
--
-- NOTE on ways to identity packages:
--
-- * "Source IDs" consist of a package name and a version: foo-1.0
-- * "Package Keys" are a hash of a package's source ID and the source IDs of
--   all its transitive dependencies. It is used internally by ghc to construct
--   FQN and to construct linker symbols.
-- * "Installed IDs" uniquely identifies a particular build of a package: its
--   version, its build time configuration flags, and the installed IDs for all
--   its transitive dependencies.
--
-- GHC cannot simultaneously load multiple packages that have different
-- installed IDs but identical package keys (amongst other things, this would
-- result in linker errors about multiply defined symbols). When GHC loads
-- packages from the package DB, it extract package keys, and it indices the
-- package DB by package key; installed IDs do not feature internally in GHC.
--
-- Prior to version 7.10 ghc used a fourth way to identify a package, which was
-- variously known as a packageId or a packageName. This package ID was
-- basically a flat string version of the Source ID. As of version 7.10 this
-- concept is replaced in favour of the package key. In the shim we paper over
-- this by calling the internal ID used by GHC a "package key" always, and map
-- that to a packageId for older versions.
--
-- In addition, we have our _own_ datatype which is (somewhat unfortunately)
-- called a PackageId; this corresponds to a source ID.
{-# LANGUAGE CPP #-}
module GhcShim (
    module GhcShim.API
#ifdef GHC_742
  , module GhcShim.GhcShim742
#endif
#ifdef GHC_78
  , module GhcShim.GhcShim78
#endif
#ifdef GHC_710
  , module GhcShim.GhcShim710
#endif
  ) where

import GhcShim.API

#ifdef GHC_742
import GhcShim.GhcShim742
#else
#ifdef GHC_78
import GhcShim.GhcShim78
#else
#ifdef GHC_710
import GhcShim.GhcShim710
#else
#error "Unsupported GHC version"
#endif
#endif
#endif
