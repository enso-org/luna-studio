{-# LANGUAGE OverloadedStrings #-}
module Object.Widget.Button where

import           Luna.Studio.Data.Vector (Position, Size)
import           Luna.Studio.Prelude

import           Data.Aeson              (ToJSON)

import qualified Object.Widget.Label     as Label
import qualified Style.Button            as Style
import           Style.Types

data Button = Button { _position :: Position
                     , _size     :: Size
                     , _label    :: Text
                     , _icon     :: Maybe Text
                     , _enabled  :: Bool
                     , _style    :: Style
                     } deriving (Eq, Show, Typeable, Generic)

data Style = Style { _background :: Color
                   , _rounded    :: Bool
                   , _alignment  :: Label.TextAlignment
                   } deriving (Eq, Show, Generic)


makeLenses ''Button
makeLenses ''Style
instance ToJSON Button
instance ToJSON Style

instance Default Style where
    def = Style Style.background Style.rounded Style.textAlignment

create :: Size -> Text -> Button
create s l = Button def s l Nothing True def

createIcon :: Size -> Text -> Button
createIcon s i = Button def s "" (Just i) True def
