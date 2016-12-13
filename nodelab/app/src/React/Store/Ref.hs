{-# LANGUAGE TypeFamilies #-}

module React.Store.Ref where

import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State  hiding (get, modify)
import qualified Control.Monad.Trans.State  as State
import           Data.Tuple                 (swap)
import           React.Flux
import           Luna.Studio.Prelude          as P hiding (transform)

import qualified Event.Event                as E
import           Reactive.Commands.Command  (Command)



type Ref a = ReactStore (Store a)

data WRef a = WRef { _ref :: Ref a
                   , _widget :: a
                   }

type SendEvent = E.Event -> IO ()

data Store a = Store { _dt :: a
                     , _sendEvent :: SendEvent
                     }

type SendEventM = ReaderT SendEvent
type StoreModifyM a m = StateT a (SendEventM m)--TODO newtype

makeLenses ''WRef
makeLenses ''Store

runStoreModifyM :: Monad m => StoreModifyM a m r -> Store a -> m (Store a, r)
runStoreModifyM action store = do
    (ret, newDt) <- runReaderT (runStateT action $ store ^. dt) (store ^. sendEvent)
    return (store & dt .~ newDt, ret)

modify :: Typeable s => (s -> (s, r)) -> Ref s -> Command a r
modify action = modifyM (StateT $ return . swap . action)

modify_ :: Typeable s => (s -> s) -> Ref s -> Command a ()
modify_ action = modifyM_ (State.modify action)

modifyM :: Typeable s => StoreModifyM s IO r -> Ref s -> Command a r
modifyM action = liftIO . flip modifyStore (runStoreModifyM action)

modifyM_ :: Typeable s => StoreModifyM s IO () -> Ref s -> Command a ()
modifyM_ = modifyM

modifyIf ::  Typeable s
         => (s -> Bool)
            -> (s -> (s, r))
            -> (s -> r)
         -> Ref s
         -> Command a r
modifyIf cond actionTrue actionFalse = modifyIfM cond (StateT $ return . swap . actionTrue) (return . actionFalse)

modifyIfM :: Typeable s
         => (s -> Bool)
            -> StoreModifyM s IO r
            -> (s -> IO r)
         -> Ref s
         -> Command a r
modifyIfM cond actionTrue actionFalse store = liftIO $ modifyStoreIf store cond' actionTrue' actionFalse' where
    cond' = cond . _dt
    actionTrue' = runStoreModifyM actionTrue
    actionFalse' = actionFalse . _dt

with :: (p -> Command a r) -> Ref p -> Command a r
with action parentRef = action =<< get parentRef

get :: Ref p -> Command s p
get rf = _dt <$> liftIO (getStoreData rf)

get' :: Ref p -> Command s (WRef p)
get' rf = WRef rf <$> get rf

use :: Getting r s r -> Ref s -> Command state r
use getter store = P.view getter <$> get store
