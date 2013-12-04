module GhcShim.API (
    -- * Traversing the AST
    IsBinder(..)
  , AstAlg(..)
  , FoldId(..)
  , Fold(..)
  ) where

import Type   (Type)
import SrcLoc (SrcSpan, Located)
import Name   (Name)
import Var    (Var)

-- | Is this a binding occurrence of @f@?
data IsBinder =
    DefSite   -- ^ @f = ..@
  | UseSite   -- ^ @g = f ..@
  | SigSite   -- ^ @f :: ..@ or fixity declaration

-- | The "algebra" that we use while traversing the AST.
--
-- This is not a general purpose fold; we look for identifiers and types.
data AstAlg m = AstAlg {
    -- | Mark a branch point in the AST
    astMark :: Maybe SrcSpan -> String -> m (Maybe Type) -> m (Maybe Type)
    -- | Throw a runtime error
  , astUnsupported :: Maybe SrcSpan -> String -> m (Maybe Type)
    -- | Found a subexpression type
  , astExpType :: SrcSpan -> Maybe Type -> m (Maybe Type)
    -- | Found a 'Name' (i.e., pre type checking)
  , astName :: Located Name -> IsBinder -> m (Maybe Type)
    -- | Found a 'Var' (i.e., post type checking)
  , astVar :: Located Var  -> IsBinder -> m (Maybe Type)
  }

class FoldId a where
  foldId   :: AstAlg m -> Located a -> IsBinder -> m (Maybe Type)
  ifPostTc :: a -> b -> Maybe b

class Fold a where
  fold :: Monad m => AstAlg m -> a -> m (Maybe Type)
