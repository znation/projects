module Encoding (byteEncode) where

import qualified Data.Bits as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Word as W
import qualified Foreign.Storable as FS

byteEncode :: Integral a => B.Bits a => FS.Storable a => a -> [W.Word8]
byteEncode i = byteEncode' (FS.sizeOf i) i

byteEncode' :: Integral a => B.Bits a => FS.Storable a => Int-> a -> [W.Word8]
byteEncode' size i
    | size == 1 = [getByte i 0]
    | size == 2 = [(getByte i 0), (getByte i 1)]
    | size == 4 = [(getByte i 0), (getByte i 1), (getByte i 2), (getByte i 3)]
    | otherwise = []

getByte :: Integral a => B.Bits a => FS.Storable a => a -> Int -> W.Word8
getByte i idx = fromInteger( toInteger(B.shiftL (B.shiftR i (8 * idx)) (8 * ((FS.sizeOf i) - idx))))
