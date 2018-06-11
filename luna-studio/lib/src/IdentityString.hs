{-# LANGUAGE JavaScriptFFI #-}

module IdentityString where

import Common.Prelude
import Data.Text      (Text)
import Unsafe.Coerce

data IdentityString = IdentityString
    { _jsString :: !JSString
    , _eqRef    :: !JSVal }

makeLenses ''IdentityString

foreign import javascript unsafe "$1===$2"
    jsValEq :: JSVal -> JSVal -> Bool

foreign import javascript safe "new Object()"
    newRef :: IO JSVal

instance Eq IdentityString where
    IdentityString _ r1 == IdentityString _ r2 = jsValEq r1 r2

fromJSString :: MonadIO m => JSString -> m IdentityString
fromJSString t = liftIO $ IdentityString t <$> newRef
