{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Object.Widget.List where

import           Data.Aeson                       (ToJSON)
import           Luna.Studio.Data.Vector          (Position, Size (Size), Vector2 (Vector2))
import           Luna.Studio.Prelude              hiding (Choice)

import           Luna.Studio.React.View.LunaValue
import           Object.Widget.Group              (Group (..))

data List = List { _position    :: Position
                 , _size        :: Size
                 , _label       :: Text
                 , _value       :: [AnyLunaValue]
                 , _empty       :: AnyLunaValue
                 , _fixedLength :: Bool
                 } deriving (Show, Typeable, Generic, Eq)

makeLenses ''List
instance ToJSON List

createList :: Double -> Text -> [AnyLunaValue] -> AnyLunaValue -> List
createList w l v e = List def (Size (Vector2 w 0)) l v e False

createTuple :: Text -> [AnyLunaValue] -> AnyLunaValue -> List
createTuple l v e = List def def l v e True

toGroup :: List -> Group
toGroup l = Group (l ^. position) (l ^. size) True def
