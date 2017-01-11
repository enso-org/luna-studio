{-# OPTIONS_GHC -fno-warn-unused-binds #-}

{-# LANGUAGE OverloadedStrings #-}

module Luna.Studio.Handler.Debug
    ( toAction
    , toActionEv
    ) where


import           JS.Debug                   (cinfo, clog, lastEv, processedEvent, saveState, shouldExportState)
import           Luna.Studio.Prelude

import qualified Event.Debug                as Debug
import           Event.Event                (Event (..))

import           Control.Monad.State        hiding (state)
import           Luna.Studio.Action.Command (Command, performIO)
import qualified Luna.Studio.State.Global   as Global

import           Data.Aeson                 (toJSON)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Debug Debug.GetState) = Just $ do
    state <- get
    let json = toJSON state
    performIO $ do
        val <- toJSVal json
        clog val
        saveState val
toAction _ev = Just $ do
    -- logBatch ev
    when shouldExportState $ do
        state <- get
        let json = toJSON state
        performIO $ do
            val <- toJSVal json
            saveState val

toActionEv :: Event -> Maybe (Command Global.State ())
toActionEv ev = Just $ do
    -- Global.lastEvent ?= ev
    -- Global.eventNum  += 1
    when shouldExportState $ do
        evN <- use $ Global.eventNum
        performIO $ do
            processedEvent evN
            val <- toJSVal $ toJSON ev
            lastEv val

logBatch :: Event -> Command Global.State ()
logBatch (Batch e) = performIO $ do
    val <- toJSVal $ toJSON e
    cinfo val
logBatch _ = return ()
