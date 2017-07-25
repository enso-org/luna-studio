{-# LANGUAGE JavaScriptFFI #-}
module GZip (compress, decompress) where

import           Common.Prelude
import           Data.ByteString                   (useAsCStringLen)
import           Data.ByteString.Lazy              (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy              as BS
import           Foreign.Ptr                       (Ptr)
import           GHCJS.Buffer                      (Buffer, MutableBuffer)
import qualified GHCJS.Buffer                      as Buffer
import           GHCJS.DOM.Types                   (Uint8Array)
import           GHCJS.Marshal.Pure                (pFromJSVal, pToJSVal)
import           GHCJS.Marshal                     (fromJSVal)
import           GHCJS.Types                       (JSVal)
import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer, MutableArrayBuffer, unsafeFreeze)
import           System.IO.Unsafe                  (unsafePerformIO)

foreign import javascript unsafe
    "(function(){return new Uint8Array($1.u8.slice(0, $2));})()"
    jsUint8Array :: Ptr a -> JSVal -> IO JSVal

foreign import javascript unsafe
    "(function(a) { return gzip.compressBytes(a); })($1)"
    jsCompress :: Uint8Array -> IO JSVal

foreign import javascript unsafe
    "(function(a) { return gzip.decompressBytes(a); })($1)"
    jsDecompress :: Uint8Array -> IO JSVal

newUint8Array :: (Ptr a, Int) -> IO Uint8Array
newUint8Array (bytes, len) = pFromJSVal <$> jsUint8Array bytes (pToJSVal len)

getArrayBuffer :: JSVal -> IO MutableArrayBuffer
getArrayBuffer = return . pFromJSVal

useGZip :: (Uint8Array -> IO JSVal) -> ByteString -> ByteString
useGZip op bytesL = do
    let bytes = toStrict bytesL
    bytesArray  <- useAsCStringLen bytes newUint8Array
    arrayBuffer <- op bytesArray >>= getArrayBuffer >>= unsafeFreeze
    let buffer = Buffer.createFromArrayBuffer arrayBuffer :: Buffer
    return . fromStrict $ Buffer.toByteString 0 Nothing buffer

compress :: ByteString -> ByteString
#ifdef COMPRESS_REQUESTS
compress = unsafePerformIO . useGZip jsCompress
#else
compress = id
#endif

decompress :: ByteString -> ByteString
#ifdef COMPRESS_REQUESTS
decompress = unsafePerformIO . useGZip jsDecompress
#else
decompress = id
#endif
