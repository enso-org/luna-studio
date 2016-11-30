module Reactive.State.UIElements where


import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector      (Vector2)

import           Object.UITypes

data State = State { _sidebar           :: WidgetId
                   , _breadcrumbs       :: WidgetId
                   , _textEditorToggle  :: WidgetId
                   , _textEditorVisible :: Bool
                   , _nsPos             :: Vector2 Double
                   } deriving (Eq, Show, Generic)


makeLenses ''State

instance ToJSON State

instance Default State where
    def = State def def def True def -- initialized in Init#initialize
