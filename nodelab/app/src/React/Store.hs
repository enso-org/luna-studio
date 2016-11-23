module React.Store where

import           React.Flux
import           Utils.PreludePlus         as P

import           Reactive.Commands.Command (Command)


modify :: Typeable s => (s -> (s, r)) -> ReactStore s -> Command a r
modify action = modifyM (return . action)

modify_ :: Typeable s => (s -> s) -> ReactStore s -> Command a ()
modify_ action = modifyM_ (return . action)

modifyM :: Typeable s => (s -> IO (s, r)) -> ReactStore s -> Command a r
modifyM = liftIO .: flip modifyStore

modifyM_ :: Typeable s => (s -> IO s) -> ReactStore s -> Command a ()
modifyM_ action = modifyM $ \s -> do
    s' <- action s
    return (s', ())

modifyIf ::  Typeable s
         => (s -> Bool)
            -> (s -> (s, r))
            -> (s -> r)
         -> ReactStore s
         -> Command a r
modifyIf cond actionTrue actionFalse = modifyIfM cond (return . actionTrue) (return . actionFalse)

modifyIfM :: Typeable s
         => (s -> Bool)
            -> (s -> IO (s, r))
            -> (s -> IO r)
         -> ReactStore s
         -> Command a r
modifyIfM cond actionTrue actionFalse store = liftIO $ modifyStoreIf store cond actionTrue actionFalse

inside :: (p -> Command a r) -> ReactStore p -> Command a r
inside action parentRef = action =<< get parentRef

get :: ReactStore p -> Command s p
get = liftIO . getStoreData

use :: Getting r s r -> ReactStore s -> Command state r
use getter store = P.view getter <$> get store
