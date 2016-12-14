{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Scene where

import Luna.Studio.Prelude
import Object.Widget
import Data.Aeson (ToJSON)

data Scene = Scene deriving (Show, Generic)

makeLenses ''Scene
instance ToJSON Scene

instance IsDisplayObject Scene where
    widgetPosition = error "Scene has no position"
    widgetSize     = error "Scene has no size"
    widgetVisible  = to $ const True

instance UIDisplayObject Scene where
    createUI _ _ _ = error "Scene has no creator"
    updateUI _ _ _ = error "Scene has no updater"

