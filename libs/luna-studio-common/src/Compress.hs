module Compression (pack, unpack) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL

type LazyByteString   = BSL.ByteString
type StrictByteString = BS.ByteString

pack :: LazyByteString -> StrictByteString
#ifdef COMPRESS_REQUESTS
pack = BS.toStrict . GZip.compress
#else
pack = BS.toStrict
#endif

unpack :: StrictByteString -> LazyByteString
#ifdef COMPRESS_REQUESTS
unpack = GZip.decompress . BS.fromStrict
#else
unpack = BS.fromStrict
#endif
