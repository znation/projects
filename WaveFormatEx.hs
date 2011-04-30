module WaveFormatEx (WaveFormatEx(WaveFormatEx, channels, samplesPerSecond), fromBytes, toByteString, create, size) where

import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Word
import Debugging
import Encoding

data WaveFormatEx = WaveFormatEx {  formatTag           :: Word16,
                                    channels            :: Word16,
                                    samplesPerSecond    :: Word32,
                                    avgBytesPerSecond   :: Word32,
                                    blockAlign          :: Word16,
                                    bitsPerSample       :: Word16 }

instance Eq WaveFormatEx where
    x == y =    (formatTag x == formatTag y) &&
                (channels x == channels y) &&
                (samplesPerSecond x == samplesPerSecond y) &&
                (avgBytesPerSecond x == avgBytesPerSecond y) &&
                (blockAlign x == blockAlign y) &&
                (bitsPerSample x == bitsPerSample y)
                                   
toByteString :: WaveFormatEx -> BSL.ByteString
toByteString wfe = BSL.concat  [(BSL.pack (byteEncode (formatTag wfe))),
                                            (BSL.pack (byteEncode (channels wfe))),
                                            (BSL.pack (byteEncode (samplesPerSecond wfe))),
                                            (BSL.pack (byteEncode (avgBytesPerSecond wfe))),
                                            (BSL.pack (byteEncode (blockAlign wfe))),
                                            (BSL.pack (byteEncode (bitsPerSample wfe)))]

fromBytes :: [Word8] -> WaveFormatEx
fromBytes bytes =   let formatTagBytes = take 2 bytes
                        formatTag = byteDecode formatTagBytes
                        channelsBytes = take 2 (drop 2 bytes)
                        channels = byteDecode channelsBytes
                        samplesPerSecondBytes = take 4 (drop 4 bytes)
                        samplesPerSecond = byteDecode samplesPerSecondBytes
                        avgBytesPerSecondBytes = take 4 (drop 8 bytes)
                        avgBytesPerSecond = byteDecode avgBytesPerSecondBytes
                        blockAlignBytes = take 2 (drop 12 bytes)
                        blockAlign = byteDecode blockAlignBytes
                        bitsPerSampleBytes = take 2 (drop 14 bytes)
                        bitsPerSample = byteDecode bitsPerSampleBytes
                    in  WaveFormatEx formatTag channels samplesPerSecond avgBytesPerSecond blockAlign bitsPerSample
                                            
create :: WaveFormatEx
create = WaveFormatEx 1 1 44100 (ceiling (2 * 44100 * 2)) (2 * 2) 16;

size :: Int32
size = 16
