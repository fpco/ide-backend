{-# LANGUAGE CPP #-}
module GhcShim (
    module GhcShim.API
#ifdef GHC_742
  , module GhcShim.GhcShim742
#endif
  ) where

import GhcShim.API

#ifdef GHC_742
import GhcShim.GhcShim742
#endif
