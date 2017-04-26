{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.State.UI where

import           Common.Prelude
import           Data.ScreenPosition        (ScreenPosition (ScreenPosition))
import           Empire.API.Data.Vector2    (Vector2 (Vector2))
import           NodeEditor.React.Model.App (App)
import           NodeEditor.React.Store     (Ref)


data State = State { _app                  :: Ref App
                   , _renderNeeded         :: Bool
                   , _mousePos             :: ScreenPosition
                   , _topZIndex            :: Int
                   }

makeLenses ''State

mkState :: Ref App -> State
mkState ref = State
    {- app          -} ref
    {- renderNeeded -} False
    {- mousePos     -} (ScreenPosition $ Vector2 200 200)
    {- topZIndex    -} def
