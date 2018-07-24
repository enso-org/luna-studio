{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Event.Filter where

import           Common.Data.Event                          (eventName)
import           Common.Prelude
import qualified Data.Text                                  as Text
import           JS.Atom                                    (acceptEvent)
import qualified JS.Event                                   as JS
import           LunaStudio.Data.Port                       (AnyPortId (InPortId', OutPortId'), isSelf)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           NodeEditor.Event.Event                     (Event (Shortcut, UI))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.Event.UI                        (UIEvent (NodeEvent, PortEvent, SearcherEvent, VisualizationEvent))
import qualified NodeEditor.React.Event.Node                as NodeEvent
import qualified NodeEditor.React.Event.Port                as PortEvent
import qualified NodeEditor.React.Event.Searcher            as Searcher
import qualified NodeEditor.React.Event.Visualization       as VisualizationEvent
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.React.Model.NodeEditor          (getExpressionNode, returnsGraphError)
import           NodeEditor.State.Global                    (State, getNodeEditor)


toJSEvent :: Event -> State -> IO JS.Event
toJSEvent evt state = JS.Event (Text.pack $ eventName evt) . nodeEditorEvent <$> getNodeEditor state where
    portRef (UI (PortEvent e))          = Just $ e ^. PortEvent.portRef
    portRef _                           = Nothing
    nodeLoc (UI (NodeEvent e))          = Just $ e ^. NodeEvent.nodeLoc
    nodeLoc (UI (VisualizationEvent e)) = e ^. VisualizationEvent.mayNodeLoc
    nodeLoc e                           = view PortRef.nodeLoc <$> portRef e
    node ne                             = maybe def (flip getExpressionNode ne) $ nodeLoc evt
    getNodeInfo ne                      = (\n -> JS.NodeInfo (n ^. Node.nodeLoc) (fromMaybe def $ n ^. Node.name) (getPortInfo n)) <$> node ne
    toPortInfo n (OutPortId' pid)       = JS.PortInfo $ "OutPort " <> convert (show $ Node.visibleOutPortNumber n pid)
    toPortInfo n (InPortId'  pid)       = JS.PortInfo $ (if isSelf pid then "Self " else "InPort ") <> convert (show $ Node.visibleInPortNumber n pid)
    getPortInfo n                       = toPortInfo n . view PortRef.portId <$> portRef evt
    getSearcherInfo (UI (SearcherEvent (Searcher.InputChanged input _ _))) = Just $ JS.SearcherInfo input
    getSearcherInfo _                   = Nothing
    nodeEditorEvent ne                  = JS.GraphEvent $ JS.GraphInfo (getNodeInfo ne) (getSearcherInfo evt)


alwaysAcceptedCommands :: [Shortcut.Command]
alwaysAcceptedCommands = [ Shortcut.Copy
                         , Shortcut.Cut
                         , Shortcut.Paste
                         , Shortcut.Undo
                         , Shortcut.Redo
                         ]

graphErrorFilter :: MonadIO m => State -> Event -> m Bool
graphErrorFilter state event = check . view returnsGraphError <$> getNodeEditor state where
    check isError = if not isError then True else case event of
        Shortcut evt -> elem (evt ^. Shortcut.shortcut) alwaysAcceptedCommands
        UI       _   -> False
        _            -> True

filterEvents :: State -> Event -> IO State -> IO State
filterEvents state event action = do
    accept <- (&&) <$> (acceptEvent <$> toJSEvent event state) <*> graphErrorFilter state event
    if accept then action else return state
