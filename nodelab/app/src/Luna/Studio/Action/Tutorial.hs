{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Action.Tutorial where

import           Control.Monad.Trans.Maybe            (runMaybeT)
import qualified Data.Text.Lazy                       as Text
import           Luna.Studio.Prelude

import qualified Empire.API.Data.DefaultValue         as DefaultValue
import           Empire.API.Data.GraphLocation        (GraphLocation)
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Data.Port                 as Port
import qualified Empire.API.Data.PortRef              as PortRef
import qualified Empire.API.Graph.Connect             as Connect
import qualified Empire.API.Graph.NodesUpdate         as NodesUpdate
import qualified Event.Batch                          as Batch
import qualified Event.CustomEvent                    as CustomEvent
import           Event.Event                          (Event (..))
import           Event.Keyboard                       (KeyMods (..))
import qualified Event.Keyboard                       as Keyboard
import qualified Event.NodeSearcher                   as NodeSearcher

import qualified Luna.Studio.Batch.Workspace          as Workspace
import           Luna.Studio.Commands.Command         (Command, performIO)
import           Luna.Studio.Commands.Graph.Selection (selectedNodes)
import           Luna.Studio.React.Store              (widget)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph
import qualified Object.Widget.Node                   as UINode

import           JS.Tutorial                          (closeOnboarding, showStep)


--  0. TAB
--  1. enter expression: readFile "/userdata/why_fp_matters.txt"
--  2. expand node to inspect details and results
--  3. unselect readFile node and add "length"
--  4. connect output of readFile to input of length
--  5. select readFile node && tab: words  -- tu warto dodac wczesniej, ze przesun node length wyzej?
--  6. tab -> map _.length
--  7. tab -> sort
--  8. tab -> histogram
--  9. create switch node
-- 10. unselect node, add "/lorem.txt" node
-- 11. unselect node, add "/ipsum.txt" node
-- 12. connect why_fp_matters to second port of switch node
-- 13. connect cakeipsum to third port of switch
-- 14. expand switch node
-- 15. connect switch to first port of readFile
-- 16. change first argument of switch node


toAction :: Event -> Maybe (Command Global.State ())
-- press tab to open node searcher
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t' KeyMods { _shift  = False })) = Just $ whenStep 0 nextStep
-- enter expression readFile "/userdata/why_fp_matters.txt"
toAction (NodeSearcher (NodeSearcher.Create "readFile \"/lorem.txt\"" Nothing)) = Just $ whenStep 1 $ andNothingIsSelected nextStep
-- expand switch node
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\r'  _))                                      = Just $ do
    whenStep  2  $ andIsSelected "readFile"  nextStep
    whenStep  4  $ andIsSelected "words"     nextStep
    whenStep  6  $ andIsSelected "map"       nextStep
    whenStep  8  $ andIsSelected "sort"      nextStep
    whenStep 10  $ andIsSelected "histogram" nextStep
-- tab > "length"
toAction (NodeSearcher (NodeSearcher.Create "words" Nothing))                                     = Just $ whenStep 3  $ andIsSelected "readFile" nextStep
-- tab > "map _.length"
toAction (NodeSearcher (NodeSearcher.Create "map _.length" Nothing))                              = Just $ whenStep 5  $ andIsSelected "words"    nextStep
-- tab > sort
toAction (NodeSearcher (NodeSearcher.Create "sort" Nothing))                                      = Just $ whenStep 7  $ andIsSelected "map"      nextStep
-- tab > histogram
toAction (NodeSearcher (NodeSearcher.Create "histogram" Nothing))                                 = Just $ whenStep 9  $ andIsSelected "sort"     nextStep
-- tab > switch
toAction (NodeSearcher (NodeSearcher.Create "length" Nothing))                                    = Just $ whenStep 11  $ andNothingIsSelected    nextStep
-- select "readFile" node and tab > "words"
-- toAction (NodeSearcher (NodeSearcher.Create (Text.stripPrefix "switch" -> Just _) Nothing))       = Just $ whenStep 9  $ andNothingIsSelected     $ nextStep
-- -- tab > switch
-- toAction (NodeSearcher (NodeSearcher.Create "\"/lorem.txt\"" Nothing))                            = Just $ whenStep 10 $ andNothingIsSelected     $ nextStep
-- toAction (NodeSearcher (NodeSearcher.Create "\"/ipsum.txt\"" Nothing))                            = Just $ whenStep 11 $ andNothingIsSelected     $ nextStep
-- -- connect switch to readFile
toAction (Batch        (Batch.NodesConnected update))                                             = Just $ do
    shouldProcess <- isCurrentLocation (update ^. Connect.location')
    when shouldProcess $
        whenStep  12 $ andConnected update "readFile"       "length"   Port.Self nextStep
--         whenStep 12 $ andConnected update "\"/lorem.txt\"" "switch"   (Port.Arg 1) $ nextStep
--         whenStep 13 $ andConnected update "\"/ipsum.txt\"" "switch"   (Port.Arg 2) $ nextStep
--         whenStep 15 $ andConnected update "switch"         "readFile" (Port.Arg 0) $ nextStep
--
toAction (Batch        (Batch.NodesUpdated update))                                                = Just $ do
    shouldProcess <- isCurrentLocation (update ^. NodesUpdate.location)
    when shouldProcess $
        whenStep 13 $ andPortDefaultChanged update "readFile" (Port.Arg 0) (DefaultValue.StringValue "/ipsum.txt") nextStep
toAction (CustomEvent (CustomEvent.RawEvent "closeOnboarding" _)) = Just $ do
    Global.tutorial .= Nothing
    performIO closeOnboarding
toAction (CustomEvent (CustomEvent.RawEvent "startOnboarding" _)) = Just $ goToStep 0
toAction _  = Nothing



whenStep :: Int -> Command Global.State () -> Command Global.State ()
whenStep ix action = do
    currentStep <- use Global.tutorial
    when (currentStep == Just ix) action

goToStep :: Int -> Command Global.State ()
goToStep ix = do
    Global.tutorial ?= ix
    performIO $ showStep ix

nextStep :: Command Global.State ()
nextStep = do
    currentStep <- use Global.tutorial
    withJust currentStep $ \currentStep -> goToStep (currentStep + 1)

andIsSelected :: Text -> Command Global.State () -> Command Global.State ()
andIsSelected prefix action = do
    selected   <- selectedNodes
    case selected of
            [wf]   -> do
                let expr = wf ^. widget . UINode.expression
                    isPrefixOk = Text.isPrefixOf prefix expr
                when isPrefixOk action
            []     -> return ()
            (_:_)  -> return ()

andNothingIsSelected :: Command Global.State () -> Command Global.State ()
andNothingIsSelected action = do
    selected   <- selectedNodes
    when (null selected) action

andConnected :: Connect.Update -> Text -> Text -> Port.InPort -> Command Global.State () -> Command Global.State ()
andConnected update expr1 expr2 portId action = void $ runMaybeT $ do
    let src = update ^. Connect.src' . PortRef.srcNodeId
        dst = update ^. Connect.dst' . PortRef.dstNodeId
        dstPortId = update ^. Connect.dst' . PortRef.dstPortId
    when (portId == dstPortId) $ do
        (Just srcExpr) <- preuse $ Global.graph . Graph.nodesMap . ix src . Node.nodeType . Node._ExpressionNode
        (Just dstExpr) <- preuse $ Global.graph . Graph.nodesMap . ix dst . Node.nodeType . Node._ExpressionNode

        when (Text.isInfixOf expr1 srcExpr && Text.isInfixOf expr2 dstExpr) $ lift action

andPortDefaultChanged :: NodesUpdate.Update -> Text -> Port.InPort -> DefaultValue.Value -> Command Global.State () -> Command Global.State ()
andPortDefaultChanged update expr portId value _action = do
    forM_ (update ^. NodesUpdate.nodes) $ \node -> do
        let nodeExpr = node ^? Node.nodeType . Node._ExpressionNode

        withJust nodeExpr $ \nodeExpr -> do
          let isExprOk = Text.isInfixOf expr nodeExpr

          when isExprOk $ do
              let portDefault = node ^? Node.ports . ix (Port.InPortId portId) . Port.state . Port._WithDefault . DefaultValue._Constant
              when (portDefault == Just value) nextStep

isCurrentLocation :: GraphLocation -> Command Global.State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)
