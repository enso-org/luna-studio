{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Searcher where

import           React.Flux                                 (keyCode, KeyboardEvent)
import           Utils.PreludePlus                          hiding (stripPrefix)

import           Event.Event                                (Event (UI))
import qualified Event.Keys                                 as Keys
import           Event.UI                                   (UIEvent (AppEvent))
import qualified React.Event.App                            as App
import qualified React.Store.Searcher                       as Searcher
import           Reactive.Commands.Command                  (Command)
import qualified Reactive.Commands.CommandSearcher.Commands as CS
import           Reactive.Commands.Node.Register            (registerNode)
import           Reactive.Commands.Node.Update              (updateExpression)
import qualified Reactive.Commands.Searcher                 as Searcher
import           Reactive.State.Global                      (State)
import qualified Reactive.State.Global                      as Global



toAction :: Event -> Maybe (Command State ())
toAction (UI (AppEvent (App.MouseDown _))) = Just $ Searcher.close
toAction (UI (AppEvent (App.KeyDown   e))) = Just $ handleKey e
toAction _ = Nothing

handleKey :: KeyboardEvent -> Command State ()
handleKey evt
    | keyCode evt == Keys.tab = Searcher.open
    | keyCode evt == Keys.esc = Searcher.close
    | otherwise               = print evt
