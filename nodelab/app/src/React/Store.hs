module React.Store where

import           React.Flux
import           Utils.PreludePlus         as P

import           Reactive.Commands.Command (Command)


type Ref = ReactStore
data WRef a = WRef { _ref :: Ref a
                   , _widget :: a
                   }

makeLenses ''WRef

modify :: Typeable s => (s -> (s, r)) -> Ref s -> Command a r
modify action = modifyM (return . action)

modify_ :: Typeable s => (s -> s) -> Ref s -> Command a ()
modify_ action = modifyM_ (return . action)

modifyM :: Typeable s => (s -> IO (s, r)) -> Ref s -> Command a r
modifyM = liftIO .: flip modifyStore

modifyM_ :: Typeable s => (s -> IO s) -> Ref s -> Command a ()
modifyM_ action = modifyM $ \s -> do
    s' <- action s
    return (s', ())

modifyIf ::  Typeable s
         => (s -> Bool)
            -> (s -> (s, r))
            -> (s -> r)
         -> Ref s
         -> Command a r
modifyIf cond actionTrue actionFalse = modifyIfM cond (return . actionTrue) (return . actionFalse)

modifyIfM :: Typeable s
         => (s -> Bool)
            -> (s -> IO (s, r))
            -> (s -> IO r)
         -> Ref s
         -> Command a r
modifyIfM cond actionTrue actionFalse store = liftIO $ modifyStoreIf store cond actionTrue actionFalse

inside :: (p -> Command a r) -> Ref p -> Command a r
inside action parentRef = action =<< get parentRef

get :: Ref p -> Command s p
get = liftIO . getStoreData

get' :: Ref p -> Command s (WRef p)
get' ref = WRef ref <$> get ref

use :: Getting r s r -> Ref s -> Command state r
use getter store = P.view getter <$> get store
