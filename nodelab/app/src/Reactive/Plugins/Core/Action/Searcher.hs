{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Searcher where

import           Utils.PreludePlus                          hiding (stripPrefix)

import           Event.Event                                (Event (..))
import           Event.Keyboard                             (KeyMods (..))
import qualified Event.Keyboard                             as Keyboard
-- import qualified Event.Searcher                             as Searcher

import           Event.Event                                (Event (UI))
import           Event.UI                                   (UIEvent (AppEvent))
import qualified React.Event.App                            as App
import qualified React.Store.Searcher                       as Searcher
import           Reactive.Commands.Command                  (Command)
import qualified Reactive.Commands.CommandSearcher.Commands as CS
import           Reactive.Commands.Node.Register            (registerNode)
import           Reactive.Commands.Node.Update              (updateExpression)
-- import qualified Reactive.Commands.Searcher                 as NS
import qualified Reactive.State.Global                      as Global


toAction :: Event -> Maybe (Command Global.State ())
toAction (UI (AppEvent (App.KeyDown e))) = Just $ print e
-- toAction (Searcher (Searcher.Query  expr)) = Just $ NS.querySearch expr
-- toAction (Searcher (Searcher.Tree   expr)) = Just $ NS.queryTree expr
-- toAction (Searcher (Searcher.Create expr nodeIdMay)) = case nodeIdMay of
--     Just nodeId -> Just $ updateExpression nodeId expr
--     Nothing     -> Just $ registerNode expr
--
-- toAction (Searcher (Searcher.QueryCmd  expr)) = Just $ CS.querySearchCmd expr
-- toAction (Searcher (Searcher.TreeCmd   expr)) = Just $ CS.queryTreeCmd expr
-- toAction (Searcher (Searcher.CreateCmd expr _)) = Just $ CS.runCommand expr
--
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   (KeyMods { _shift = False }))) = Just $ NS.openFresh
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods False False False False))) = Just $ NS.openCommand -- 191 = /
-- toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods True  False False False))) = Just $ CS.help
toAction _ = Nothing
