module Object.UITypes where

import           Data.Aeson        (FromJSON, ToJSON)
import           Luna.Studio.Prelude


newtype WidgetId = WidgetId { fromWidgetId :: Int } deriving (Show, Eq, ToJSON, Default, FromJSON, Ord)

data SceneType   = HUD
                 | Workspace
                 deriving (Show, Eq, Enum, Generic)


instance ToJSON SceneType
