-- | A \"future\" datatype with intermediate progress information.
module Progress
  ( Progress(..)
  , bimapProgress
  , progressWaitCompletion, progressWaitConsume
  ) where

-- | A future, a handle on an action that will produce some result.
-- Also provides optional intermediate advancement information.
newtype Progress p a = Progress {
    progressWait :: IO (Either a (p, Progress p a))
  }

-- | The @bimap@ of a bifunctor (we don't use <http://hackage.haskell.org/packages/archive/bifunctors/3.0/doc/html/Data-Bifunctor.html> to avoid extensive dependencies).
bimapProgress :: (p -> q) -> (a -> b) -> Progress p a -> Progress q b
bimapProgress f g (Progress pr) = Progress $ do
  lr <- pr
  case lr of
    Left a -> return $ Left $ g a
    Right (p, progress2) -> return $ Right (f p, bimapProgress f g progress2)

-- | Block until the operation completes.
progressWaitCompletion :: Progress a b -> IO b
progressWaitCompletion p = do
  w <- progressWait p
  case w of
    Left a -> return a
    Right (_, p2) -> progressWaitCompletion p2

-- | Wait until the operation completes, consuming intermediate advancement
-- information whenever it arrives.
progressWaitConsume :: (a -> IO ()) -> Progress a b -> IO b
progressWaitConsume consume p = do
  w <- progressWait p
  case w of
    Left a -> return a
    Right (adv, p2) -> do
      consume adv
      progressWaitConsume consume p2
