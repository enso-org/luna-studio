{-# LANGUAGE JavaScriptFFI #-}
module GZip (compress, decompress) where

import           Common.Prelude
import           Data.ByteString      (useAsCStringLen)
import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BS
import           Foreign.Ptr          (Ptr)
import           GHCJS.Buffer         (Buffer, MutableBuffer)
import qualified GHCJS.Buffer         as Buffer
import           GHCJS.DOM.Types      (Uint8Array)
import           GHCJS.Marshal.Pure   (pFromJSVal, pToJSVal)
import           GHCJS.Marshal        (fromJSVal)
import           GHCJS.Types          (JSVal)

import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer, MutableArrayBuffer, unsafeFreeze)
import System.IO (hFlush, stdout)

foreign import javascript unsafe
    "(function(){return new Uint8Array($1.u8.slice(0, $2));})()"
    jsUint8Array :: Ptr a -> JSVal -> IO JSVal

foreign import javascript unsafe
    "(function(a) { var b = gzip.compressBytes(a); return b;})($1)"
    jsCompress :: Uint8Array -> IO JSVal

foreign import javascript unsafe
    "(function(a) { var b = gzip.decompressBytes(a); return b;})($1)"
    jsDecompress :: Uint8Array -> IO JSVal

newUint8Array :: (Ptr a, Int) -> IO Uint8Array
newUint8Array (bytes, len) = pFromJSVal <$> jsUint8Array bytes (pToJSVal len)

getArrayBuffer :: JSVal -> IO MutableArrayBuffer
getArrayBuffer = return . pFromJSVal

compress :: ByteString -> IO ByteString
compress bytesL = do
    let bytes = toStrict bytesL
    bytesArray <- useAsCStringLen bytes newUint8Array
    putStrLn "bytesArrayuyyyyyy" >> hFlush stdout
    arrayBuffer <- jsCompress bytesArray >>= getArrayBuffer >>= unsafeFreeze
    let buffer  = Buffer.createFromArrayBuffer arrayBuffer :: Buffer
    putStrLn "compressed" >> hFlush stdout
    return . fromStrict $ Buffer.toByteString 0 Nothing buffer

decompress :: ByteString -> IO ByteString
decompress bytesL = do
   let bytes = toStrict bytesL
   bytesArray <- useAsCStringLen bytes newUint8Array
   putStrLn "dupadupadupadupa" >> hFlush stdout
   arrayBuffer <- jsDecompress bytesArray >>= getArrayBuffer >>= unsafeFreeze
   putStrLn "duuuuuuuuuuuuupa" >> hFlush stdout
   let buffer = Buffer.createFromArrayBuffer arrayBuffer :: Buffer
   putStrLn "sraka"
   return . fromStrict $ Buffer.toByteString 0 Nothing buffer
