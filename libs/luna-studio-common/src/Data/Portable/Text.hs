{-# LANGUAGE CPP #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI #-}
#endif

module Data.Portable.Text where

import Prologue

#ifdef ghcjs_HOST_OS

import           Data.JSString                     (JSString)
import qualified Data.JSString                     as JSString
import qualified Data.JSString.Text                as JSString
import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import qualified GHCJS.Buffer                      as Buffer
import           Data.ByteString                   (ByteString)
import           Data.Binary                       (Binary, put, get)
import           Data.Aeson.Types                  ( ToJSON
                                                   , FromJSON
                                                   , toJSON
                                                   , parseJSON)

#else

import           Data.Text        (Text)

#endif


#ifdef ghcjs_HOST_OS

type PortableText = JSString

foreign import javascript unsafe "new TextDecoder('utf-8').decode(new DataView($1, $2, $3))"
    js_decodeUtf8 :: ArrayBuffer -> Int -> Int -> JSString

foreign import javascript unsafe "new TextEncoder().encode($1)"
    js_encodeUtf8 :: JSString -> ArrayBuffer

encodeUtf8 :: JSString -> ByteString
encodeUtf8 = Buffer.toByteString 0 Nothing
           . Buffer.createFromArrayBuffer
           . js_encodeUtf8

decodeUtf8 :: ByteString -> JSString
decodeUtf8 bs = js_decodeUtf8 arrBuf off len where
    (buf, off, len) = Buffer.fromByteString bs
    arrBuf = Buffer.getArrayBuffer buf

instance Binary JSString where
    put = put . encodeUtf8
    get = decodeUtf8 <$> get

instance ToJSON JSString where
    toJSON   = toJSON . JSString.textFromJSString

instance FromJSON JSString where
    parseJSON = fmap JSString.textToJSString . parseJSON

#else

type PortableText = Text

#endif
