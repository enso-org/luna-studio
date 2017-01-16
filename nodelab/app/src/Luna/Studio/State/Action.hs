module Luna.Studio.State.Action where

import           Data.Aeson                       (ToJSON)
import           Luna.Studio.Prelude
import qualified Luna.Studio.State.Connect        as Connect
import qualified Luna.Studio.State.Drag           as Drag
import qualified Luna.Studio.State.MultiSelection as MultiSelection
import qualified Luna.Studio.State.PanDrag        as PanDrag
import qualified Luna.Studio.State.PenConnect     as PenConnect
import qualified Luna.Studio.State.PenDisconnect  as PenDisconnect
import qualified Luna.Studio.State.Slider         as Slider
import qualified Luna.Studio.State.ZoomDrag       as ZoomDrag


data Action = Drag           Drag.State
            | MultiSelection MultiSelection.State
            | PanDrag        PanDrag.State
            | ZoomDrag       ZoomDrag.State
            | Slider         Slider.State
            | PenConnect     PenConnect.State
            | PenDisconnect  PenDisconnect.State
            | Connect        Connect.State
            deriving (Eq, Generic, Show)

instance ToJSON Action
makeLenses ''Action
