module IdeSession.Types.Progress (
    Progress(..)
  ) where

import Data.Binary (Binary(..))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import IdeSession.Util () -- instance Binary Text

-- | This type represents intermediate progress information during compilation.
data Progress = Progress {
    progressStep     :: Int
  , progressNumSteps :: Int
  , progressMsg      :: Maybe Text
  }
  deriving (Show, Eq, Ord)

instance Binary Progress where
  put (Progress {..}) = do put progressStep
                           put progressNumSteps
                           put progressMsg
  get = Progress <$> get <*> get <*> get
