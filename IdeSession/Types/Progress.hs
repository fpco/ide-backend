module IdeSession.Types.Progress (
    Progress(..)
  ) where

import Control.Applicative ((<$>), (<*>), (<|>))
import Data.Binary (Binary(..))
import Data.Text (Text)
import Data.Maybe (fromJust)
import qualified Data.Text as Text
import IdeSession.Util () -- instance Binary Text

-- | This type represents intermediate progress information during compilation.
data Progress = Progress {
    -- | The current step number
    --
    -- When these Progress messages are generated from progress updates from
    -- ghc, it is entirely possible that we might get step 4/26, 16/26, 3/26;
    -- the steps may not be continuous, might even be out of order, and may
    -- not finish at X/X.
    progressStep :: Int

    -- | The total number of steps
  , progressNumSteps :: Int

    -- | The parsed message. For instance, in the case of progress messages
    -- during compilation, 'progressOrigMsg' might be
    --
    -- > [1 of 2] Compiling M (some/path/to/file.hs, some/other/path/to/file.o)
    --
    -- while 'progressMsg' will just be 'Compiling M'
  , progressParsedMsg :: Maybe Text

    -- | The full original message (see 'progressMsg')
  , progressOrigMsg :: Maybe Text
  }
  deriving (Eq, Ord)

instance Binary Progress where
  put (Progress {..}) = do put progressStep
                           put progressNumSteps
                           put progressParsedMsg
                           put progressOrigMsg
  get = Progress <$> get <*> get <*> get <*> get

instance Show Progress where
  show (Progress{..}) =
         "["
      ++ show progressStep
      ++ " of "
      ++ show progressNumSteps
      ++ "]"
      ++ fromJust (pad progressParsedMsg <|> pad progressOrigMsg <|> Just "")
    where
      pad :: Maybe Text -> Maybe String
      pad = fmap $ \t -> " " ++ Text.unpack t
