module Reactive.State.UIElements where


import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector      (Vector2)



data State = State { _nsPos             :: Vector2 Double
                   } deriving (Eq, Show, Generic)


makeLenses ''State

instance ToJSON State

instance Default State where
    def = State def
