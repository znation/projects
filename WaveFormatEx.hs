module WaveFormatEx (WaveFormatEx, toByteString, create, size) where

import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Word
import Encoding

data WaveFormatEx = WaveFormatEx {  formatTag           :: Word16,
                                    channels            :: Word16,
                                    samplesPerSecond    :: Word32,
                                    avgBytesPerSecond   :: Word32,
                                    blockAlign          :: Word16,
                                    bitsPerSample       :: Word16 }

toByteString :: WaveFormatEx -> BSL.ByteString
toByteString wfe = BSL.concat  [(BSL.pack (byteEncode (formatTag wfe))),
                                            (BSL.pack (byteEncode (channels wfe))),
                                            (BSL.pack (byteEncode (samplesPerSecond wfe))),
                                            (BSL.pack (byteEncode (avgBytesPerSecond wfe))),
                                            (BSL.pack (byteEncode (blockAlign wfe))),
                                            (BSL.pack (byteEncode (bitsPerSample wfe)))]
                                            
create :: WaveFormatEx
create = WaveFormatEx 1 2 44100 (ceiling (2 * 44100 * 2)) (2 * 2) 16;

size :: Int32
size = 16
