{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Visualization where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command            (Command)
import qualified Luna.Studio.Action.Visualization      as Visualization
import           Luna.Studio.Event.Event               (Event (UI))
import           Luna.Studio.Event.UI                  (UIEvent (VisualizationEvent))
import qualified Luna.Studio.React.Event.Visualization as Visualization
import           Luna.Studio.State.Global              (State)



handle :: Event -> Maybe (Command State ())
handle (UI (VisualizationEvent (Visualization.Pin   nodeId visIx))) = Just $ Visualization.pin   nodeId visIx
handle (UI (VisualizationEvent (Visualization.Unpin nodeId visIx))) = Just $ Visualization.unpin nodeId visIx
handle _ = Nothing
