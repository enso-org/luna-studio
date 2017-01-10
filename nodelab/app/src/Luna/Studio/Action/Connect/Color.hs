module Luna.Studio.Action.Connect.Color
    ( getConnectionColor
    ) where

import           Empire.API.Data.PortRef         (AnyPortRef (OutPortRef'), OutPortRef)
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.Graph.Lookup (getPort)
import           Luna.Studio.Data.Color          (Color)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Port    as Port
import           Luna.Studio.State.Global        (State)


class HasColor a where
    getConnectionColor :: a -> Command State (Maybe Color)
instance HasColor OutPortRef where
    getConnectionColor = getConnectionColor . OutPortRef'
instance HasColor AnyPortRef where
    getConnectionColor portRef = (fmap $ view Port.color) <$> (getPort portRef)
