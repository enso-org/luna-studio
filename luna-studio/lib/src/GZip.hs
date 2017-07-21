{-# LANGUAGE JavaScriptFFI #-}
module GZip (compress) where

import           Common.Prelude
import           Data.ByteString      (useAsCStringLen)
import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BS
import           Foreign.Ptr          (Ptr)
import           GHCJS.Buffer         (Buffer)
import qualified GHCJS.Buffer         as Buffer
import           GHCJS.DOM.Types      (Uint8Array)
import           GHCJS.Marshal.Pure   (pFromJSVal, pToJSVal)
import           GHCJS.Types          (JSVal)

import System.IO (hFlush, stdout)

foreign import javascript unsafe
    "(function(){return new Uint8Array($1.u8.slice(0, $2));})()"
    jsUint8Array :: Ptr a -> JSVal -> IO JSVal



foreign import javascript unsafe
    "gzip.getBuffer();"
    jsBuffer :: IO Buffer

foreign import javascript unsafe
    "return gzip.getBytes();"
    jsBytes :: IO JSVal

foreign import javascript unsafe
    "gzip.compressBytes($1);"
    jsCompress :: Uint8Array -> IO JSVal

foreign import javascript unsafe
    "gzip.printCompressed();"
    jsPrintCompressed :: IO ()

jsInt :: JSVal -> Int
jsInt = pFromJSVal

newUint8Array :: (Ptr a, Int) -> IO Uint8Array
newUint8Array (bytes, len) = pFromJSVal <$> jsUint8Array bytes (pToJSVal len)

getBytes :: IO Uint8Array
getBytes = pFromJSVal <$> jsBytes

compress :: ByteString -> IO ByteString
compress bytesL = do
    let bytes = toStrict bytesL
    bytesArray <- useAsCStringLen bytes newUint8Array
    putStrLn "bytesArray" >> hFlush stdout
    jsCompress   bytesArray
    putStrLn "compressed" >> hFlush stdout
    jsPrintCompressed
    bytes      <- jsBytes
    buffer     <- jsBuffer
    print $ Buffer.byteLength buffer
    return . fromStrict $ Buffer.toByteString 0 Nothing buffer
