{-# OPTIONS_GHC -fno-warn-orphans #-}
module NodeEditor.State.UI where

import           Data.ScreenPosition         (ScreenPosition (ScreenPosition))
import           Data.Vector2                 (Vector2 (Vector2))
import           Common.Prelude
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
