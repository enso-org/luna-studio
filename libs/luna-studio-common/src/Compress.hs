{-# LANGUAGE CPP #-}
module Compress (pack, unpack) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Prologue

type LazyByteString   = BSL.ByteString
type StrictByteString = BS.ByteString

pack :: LazyByteString -> StrictByteString
#ifdef COMPRESS_REQUESTS
pack = BSL.toStrict . GZip.compress
#else
pack = BSL.toStrict
#endif

unpack :: StrictByteString -> LazyByteString
#ifdef COMPRESS_REQUESTS
unpack = GZip.decompress . BSL.fromStrict
#else
unpack = BSL.fromStrict
#endif
