{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module ZMQ.Bus.Util where

import           Data.Binary        (Binary)

import           Prelude            (fail)
import           Prologue           hiding (fail)
import           System.Log.MLogger as L
import           ZMQ.Bus.Bus        (Bus)
import qualified ZMQ.Bus.Bus        as Bus
import           ZMQ.Bus.Data.Topic (Topic)
import qualified ZMQ.Bus.Data.Topic as Topic
import qualified ZMQ.Bus.RPC.Client as Client

import           Debug.Trace        as T


logger :: Logger
logger = getLogger $moduleName


data Ping = forall arg result. (Typeable arg, Binary arg, Typeable result, Binary result) =>
    Ping { _topic  :: Topic
         , _arg    :: arg
         , _result :: result
         }


exists :: String -> Ping -> Bus Bool
exists pluginName (Ping topic request result) = do
    let topicBase = Topic.base topic
    logger info "Testing for duplicates..."
    return False
    --Bus.withTimeout (Client.query pluginName topic request) 1000000 >>= \case
    --    Left  _ -> do
    --        T.trace "KD!!" $ return ()
    --        Bus.unsubscribe topicBase >> return False
    --    Right r -> do
    --        T.trace "DK" $ return ()
    --        ofSameType r [result] $ return True
    --where
    --    ofSameType :: a -> a -> b -> b
    --    ofSameType _ _ = id


quitIfExists :: String -> Ping -> Bus ()
quitIfExists pluginName ping =
    whenM (exists pluginName ping) $ fail "bus plugin already exist, quitting..."
