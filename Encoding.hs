module Encoding (byteDecode, byteEncode) where

import qualified Data.Bits as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Int as I
import qualified Data.Word as W
import qualified Foreign.Storable as FS

byteEncode :: Integral a => B.Bits a => FS.Storable a => a -> [W.Word8]
byteEncode i = byteEncode' (FS.sizeOf i) i

byteEncode' :: Integral a => B.Bits a => FS.Storable a => Int-> a -> [W.Word8]
byteEncode' size i
    | size == 1 = [getByte i 0]
    | size == 2 = [(getByte i 0), (getByte i 1)]
    | size == 4 = [(getByte i 0), (getByte i 1), (getByte i 2), (getByte i 3)]
    | otherwise = error ("Too many bytes to encode: " ++ (show i))

getByte :: (Integral a, B.Bits a, FS.Storable a) => a -> Int -> W.Word8
getByte i idx = fromIntegral (B.shift i ((-8) * idx)) B..&. 0xFF

byteDecode :: (Num a, Integral a, B.Bits a, FS.Storable a) => [W.Word8] -> a         
byteDecode bytes
    | size == 1 = fromInteger (toInteger (bytes !! 0))
    | size == 2 = fromInteger ((toInteger (bytes !! 0)) B..|. (B.shift (toInteger (bytes !! 1)) (8 * 1)))
    | size == 4 = fromInteger ((toInteger (bytes !! 0)) B..|.
                       (B.shift (toInteger (bytes !! 1)) (8 * 1)) B..|.
                       (B.shift (toInteger (bytes !! 2)) (8 * 2)) B..|.
                       (B.shift (toInteger (bytes !! 3)) (8 * 3)))
    | otherwise = error ("Too many bytes to decode: " ++ (show bytes))
    where   size = length bytes

-- TODO : test whether decoding works and make byteDecode more succinct