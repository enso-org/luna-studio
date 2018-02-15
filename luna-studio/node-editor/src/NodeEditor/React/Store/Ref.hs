{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Store.Ref where

import           Common.Prelude             as P hiding (transform)
import           Control.Monad.State        (runState)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State  hiding (get, modify)
import           React.Flux

import           Common.Action.Command      (Command)
import qualified NodeEditor.Event.Event     as E


type Ref a = ReactStore (Store a)

type SendEvent = E.Event -> IO ()

data Store a = Store { _dt :: a
                     , _sendEvent :: SendEvent
                     } deriving (Generic, NFData)

instance Eq a => Eq (Store a) where
    s1 == s2 = _dt s1 == _dt s2

type SendEventM = ReaderT SendEvent
type StoreModifyM a m = StateT a (SendEventM m)--TODO newtype

makeLenses ''Store

runState' :: State a r -> Store a -> (Store a, r)
runState' action store = (store & dt .~ newDt, ret) where
    (ret, newDt) = runState action $ store ^. dt

continueModify :: Typeable s => State s r -> Ref s -> Command a r
continueModify action = liftIO . flip modifyStoreNoCommit (return . runState' action)

commit :: Typeable s => Ref s -> Command a ()
commit = liftIO . storeCommit

get :: MonadIO m => Ref p -> m p
get rf = _dt <$> liftIO (getStoreData rf)
