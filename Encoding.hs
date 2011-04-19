module Encoding (byteEncode) where

import qualified Data.Word as W
import qualified Foreign.Marshal as FM
import qualified Foreign.Marshal.Utils as FU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

byteEncode :: FS.Storable a => a -> [W.Word8]
byteEncode i = byteEncode' (FS.sizeOf i) i

byteEncode' :: FS.Storable a => Int-> a -> [W.Word8]
byteEncode' size i
    | size == 1 = [getByte i 0]
    | size == 2 = [(getByte i 0), (getByte i 1)]
    | size == 4 = [(getByte i 0), (getByte i 1), (getByte i 2), (getByte i 3)]
    | otherwise = []

getByte :: FS.Storable a => a -> Int -> W.Word8
getByte i idx = FM.unsafeLocalState (FU.with i (getByte' (idx * 8)))

getByte' :: (FS.Storable a, FS.Storable b) => Int -> FP.Ptr a -> IO b
getByte' idx i = FS.peekByteOff i idx
