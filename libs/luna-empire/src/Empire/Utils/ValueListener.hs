module Empire.Utils.ValueListener where

import Prologue

import qualified Luna.Runtime as Runtime
import qualified Luna.Std     ()

data SingleRep = ErrorRep Text | SuccessRep Text (Maybe Text)
data ValueRep  = OneTime SingleRep | Streaming ((SingleRep -> IO ()) -> IO ())

feedActionToLuna :: (Runtime.Data -> Runtime.Eff ()) -> Runtime.Value
feedActionToLuna act = pure $ Runtime.Function $ \val -> do
    val >>= act
    pure $ Runtime.Cons $ Runtime.Object "Internal" "Internal" (Runtime.Constructor "Internal" []) def

listenStream :: Runtime.Data -> (SingleRep -> IO ()) -> IO ()
listenStream val listener = do
    listenerSuccess <- Runtime.runIO $ Runtime.runError $ do
        each <- Runtime.tryDispatchMethod "each" val
        case each of
            Just e -> void $ Runtime.applyFun e $ feedActionToLuna (makeReps >=> Runtime.unsafeLiftIO . listener)
            Nothing -> pure ()
    case listenerSuccess of
        Left err -> listener $ ErrorRep $ unwrap err
        Right _  -> pure ()

makeReps :: Runtime.Data -> Runtime.Eff SingleRep
makeReps val = do
    short' <- Runtime.tryDispatchMethod "shortRep" val
    short  <- maybe (pure "") (>>= Runtime.fromData) short'
    long'  <- Runtime.tryDispatchMethods ["toJSON", "render"] val
    long   <- traverse (>>= Runtime.fromData) long'
    pure $ SuccessRep short long

getReps' :: Runtime.Data -> Runtime.Eff ValueRep
getReps' val = do
    forced <- Runtime.force' val
    shouldStream <- Runtime.tryDispatchMethod "isStream" forced >>= traverse (>>= Runtime.fromData)
    case shouldStream of
        Just True -> pure $ Streaming $ listenStream forced
        _         -> OneTime <$> makeReps forced

getReps :: Runtime.Data -> IO ValueRep
getReps val = do
    result <- Runtime.runIO $ Runtime.runError $ getReps' val
    case result of
        Left err  -> pure $ OneTime $ ErrorRep $ unwrap err
        Right res -> pure res
