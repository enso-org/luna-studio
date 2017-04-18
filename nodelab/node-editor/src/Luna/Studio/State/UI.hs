{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.State.UI where

import           Data.ScreenPosition         (ScreenPosition (ScreenPosition))
import           Data.Vector                 (Vector2 (Vector2))
import           Luna.Prelude
import           Luna.Studio.React.Model.App (App)
import           Luna.Studio.React.Store     (Ref)


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
