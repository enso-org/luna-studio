{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.Action.Camera.Persistence
     ( saveCamera
     , tryLoadCamera
     ) where

import           Common.Prelude
import           Control.Monad.Trans.Maybe           (MaybeT (MaybeT), runMaybeT)
import           LunaStudio.Data.GraphLocation       (filePath)
import qualified JS.LocalStorage                     as JS
import           NodeEditor.Action.Basic.CenterGraph (centerGraph)
import           NodeEditor.Action.Command           (Command)
import           NodeEditor.Action.State.NodeEditor  (modifyNodeEditor)
import           NodeEditor.Action.State.NodeEditor  (getScreenTranform)
import           NodeEditor.Batch.Workspace          (currentLocation)
import           NodeEditor.React.Model.NodeEditor   (screenTransform)
import           NodeEditor.State.Global             (State, workspace)

saveCamera :: Command State ()
saveCamera = do
    camera <- getScreenTranform
    withJustM (preuse $ workspace . traverse . currentLocation . filePath) $ \path ->
        liftIO $ JS.saveCamera path camera

tryLoadCamera :: Command State ()
tryLoadCamera = maybe centerGraph return =<< runMaybeT (do
    path <- MaybeT $ preuse $ workspace . traverse . currentLocation . filePath
    camera <- MaybeT $ liftIO $ JS.loadCamera path
    lift $ modifyNodeEditor $ screenTransform .= camera)
