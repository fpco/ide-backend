{-# LANGUAGE MultiParamTypeClasses, GADTs, FlexibleInstances #-}
module GhcShim.API (
    -- * Traversing the AST
    IsBinder(..)
  , AstAlg(..)
  , Fold(..)
  , FoldPhase(..)
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
data AstAlg m id = AstAlg {
    -- | Mark a branch point in the AST
    astMark :: Maybe SrcSpan -> String -> m (Maybe Type) -> m (Maybe Type)
    -- | Throw a runtime error
  , astUnsupported :: Maybe SrcSpan -> String -> m (Maybe Type)
    -- | Found a subexpression type
  , astExpType :: SrcSpan -> Maybe Type -> m (Maybe Type)
    -- | Found a 'Name' (i.e., pre type checking)
  , astId :: Located id -> IsBinder -> m (Maybe Type)
    -- | Are we running pre or post type checking?
  , astPhase :: FoldPhase id
  }

class Fold id a where
  fold :: Monad m => AstAlg m id -> a -> m (Maybe Type)

data FoldPhase id where
  FoldPreTc  :: FoldPhase Name
  FoldPostTc :: FoldPhase Var

instance Fold id a => Fold id [a] where
  fold alg xs = do
    mapM_ (fold alg) xs
    return Nothing

instance Fold id a => Fold id (Maybe a) where
  fold _alg Nothing  = return Nothing
  fold  alg (Just x) = fold alg x
