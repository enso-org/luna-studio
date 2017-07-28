{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module JS.Node where

import           Common.Action.Command         (Command)
import           Common.Prelude
import           Data.ScreenPosition           (fromDoubles)
import qualified JS.Config                     as Config
import qualified JS.Scene                      as Scene
import           LunaStudio.Data.Node          (NodeId)
import           LunaStudio.Data.Position      (Position)
import           NodeEditor.Action.State.Scene (translateToWorkspace)
import           NodeEditor.React.Model.Layout (Scene)
import qualified NodeEditor.React.Model.Layout as Scene
import           NodeEditor.State.Global       (State)


expandedNodeRectangle :: Scene -> NodeId -> Command State (Position, Position)
expandedNodeRectangle scene nid = do
    let scenePos = scene ^. Scene.position
    left        <- liftIO $ Scene.elementLeft   $ Config.prefix $ fromString $ "node-body-" <> show nid
    right       <- liftIO $ Scene.elementRight  $ Config.prefix $ fromString $ "node-body-" <> show nid
    top         <- liftIO $ Scene.elementTop    $ Config.prefix $ fromString $ "node-body-" <> show nid
    bottom      <- liftIO $ Scene.elementBottom $ Config.prefix $ fromString $ "node-body-" <> show nid
    leftTop     <- translateToWorkspace $ fromDoubles left  top - scenePos
    rightBottom <- translateToWorkspace $ fromDoubles right bottom - scenePos
    return (leftTop, rightBottom)
