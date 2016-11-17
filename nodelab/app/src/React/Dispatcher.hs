module React.Dispatcher (
    dispatch
    ) where

import           React.Flux
import qualified React.Store.Nodelab as Nodelab



dispatch :: Nodelab.Action -> ViewEventHandler
dispatch a = []
