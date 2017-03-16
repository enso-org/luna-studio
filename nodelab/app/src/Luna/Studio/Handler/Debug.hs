-- TODO[LJK, PM]: Remove or restore
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Handler.Debug
    ( handle
    , handleEv
    ) where


import           JS.Debug                   (cinfo, clog, lastEv, processedEvent, saveState, shouldExportState)
import           Luna.Studio.Prelude

import qualified Luna.Studio.Event.Debug    as Debug
import           Luna.Studio.Event.Event    (Event (..))

import           Control.Monad.State        hiding (state)
import           Luna.Studio.Action.Command (Command)
import qualified Luna.Studio.State.Global   as Global

import           Data.Aeson                 (toJSON)

handle :: Event -> Maybe (Command Global.State ())
-- handle (Debug Debug.GetState) = Just $ do
--     state <- get
--     let json = toJSON state
--     liftIO $ do
--         val <- toJSVal json
--         clog val
--         saveState val
-- handle _ev = Just $ do
--     -- logBatch ev
--     when shouldExportState $ do
--         state <- get
--         let json = toJSON state
--         liftIO $ do
--             val <- toJSVal json
--             saveState val
handle _ = Nothing

handleEv :: Event -> Maybe (Command Global.State ())
handleEv ev = Just $
    -- Global.lastEvent ?= ev
    -- Global.eventNum  += 1
    when shouldExportState $ do
        evN <- use Global.eventNum
        liftIO $ do
            processedEvent evN
            val <- toJSVal $ toJSON ev
            lastEv val

logBatch :: Event -> Command Global.State ()
logBatch (Batch e) = liftIO $ do
    val <- toJSVal $ toJSON e
    cinfo val
logBatch _ = return ()
