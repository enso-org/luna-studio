module Luna.Studio.Handler.Collaboration where

import qualified Data.DateTime                                as DT
import qualified Data.HashMap.Strict                          as HashMap
import qualified Data.Map.Lazy                                as Map
import           Empire.API.Data.GraphLocation                (GraphLocation)
import qualified Empire.API.Graph.CollaborationUpdate         as CollaborationUpdate
import           Luna.Studio.Action.Batch                     (collaborativeTouch)
import           Luna.Studio.Action.Command                   (Command)
import           Luna.Studio.Action.Graph                     (selectedNodes)
import           Luna.Studio.Action.Graph.CollaborationUpdate (everyNSeconds, expireTouchedNodes, refreshTime, touchCurrentlySelected)
import qualified Luna.Studio.Batch.Workspace                  as Workspace
import           Luna.Studio.Event.Batch                      (Event (..))
import qualified Luna.Studio.Event.Event                      as Event
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node                 as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor           as NodeEditor
import           Luna.Studio.State.Collaboration              (ColorId)
import           Luna.Studio.State.Global                     (State)
import qualified Luna.Studio.State.Global                     as Global

handle :: Event.Event -> Maybe (Command State ())
handle Event.Tick = Just $ expireTouchedNodes >> everyNSeconds refreshTime touchCurrentlySelected
handle _ = Nothing
