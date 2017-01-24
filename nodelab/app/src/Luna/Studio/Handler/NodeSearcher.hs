{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.NodeSearcher where

import           Luna.Studio.Prelude        hiding (stripPrefix)
--
import           Luna.Studio.Event.Event    (Event (..))
-- import           Event.Keyboard                             (KeyMods (..))
-- import qualified Event.Keyboard                             as Keyboard
-- import qualified Event.NodeSearcher                         as NodeSearcher
--
import           Luna.Studio.Action.Command (Command)
-- import qualified Luna.Studio.Action.CommandSearcher.Commands as CS
-- import           Luna.Studio.Action.Node                     (registerNode, updateExpression)
-- import qualified Luna.Studio.Action.NodeSearcher             as NS
import qualified Luna.Studio.State.Global   as Global
--
--TODO[react]
toAction :: Event -> Maybe (Command Global.State ())
-- toAction (NodeSearcher (NodeSearcher.Query  expr)) = Just $ NS.querySearch expr
-- toAction (NodeSearcher (NodeSearcher.Tree   expr)) = Just $ NS.queryTree expr
-- toAction (NodeSearcher (NodeSearcher.Create expr nodeIdMay)) = case nodeIdMay of
--     Just nodeId -> Just $ updateExpression nodeId expr
--     -- Nothing     -> Just $ registerNode expr --TODO[react]
--
-- toAction (NodeSearcher (NodeSearcher.QueryCmd  expr)) = Just $ CS.querySearchCmd expr
-- toAction (NodeSearcher (NodeSearcher.TreeCmd   expr)) = Just $ CS.queryTreeCmd expr
-- toAction (NodeSearcher (NodeSearcher.CreateCmd expr _)) = Just $ CS.runCommand expr
--
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   (KeyMods { _shift = False }))) = Just $ NS.openFresh
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods False False False False))) = Just $ NS.openCommand -- 191 = /
toAction _ = Nothing
