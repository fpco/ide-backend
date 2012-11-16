-- A "Future" datatype with intermediate progress information.
module Progress
  ( Progress(..)
  , fmap2Progress
  , progressWaitCompletion, progressWaitConsume
  ) where

-- | A future, a handle on an action that will produce some result.
-- Provides also optional intermediate advancement information.
newtype Progress p a = Progress {
    progressWait :: IO (Either a (p, Progress p a))
  }

fmap2Progress :: (p -> q) -> (a -> b) -> Progress p a -> Progress q b
fmap2Progress f g (Progress pr) = Progress $ do
  lr <- pr
  case lr of
    Left a -> return $ Left $ g a
    Right (p, progress2) -> return $ Right (f p, fmap2Progress f g progress2)

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
