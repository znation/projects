import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Bits
import Data.Int
import Data.Word
import System.IO

data WaveFormatEx = WaveFormatEx {  formatTag           :: Word16,
                                    channels            :: Word16,
                                    samplesPerSecond    :: Word32,
                                    avgBytesPerSecond   :: Word32,
                                    blockAlign          :: Word16,
                                    bitsPerSample       :: Word16 }

data WaveFile = WaveFile {  waveFormatEx        :: WaveFormatEx,
                            dataBytes           :: [Word8] }
                 
-- | 'main' runs the main program
main :: IO ()
main = do   writeWaveFile makeWave "output.wav"

-- TODO: the 0 after RIFF should be the size of the rest of the file in bytes
-- TODO: the 0 after data should be the size of the data in bytes
makeWave :: WaveFile
makeWave = WaveFile makeWaveFormatEx randomBytes

waveFormatExSize :: Int32
waveFormatExSize = 16

riffStr = "RIFF"
waveStr = "WAVE"
fmtStr = "fmt "
dataStr = "data"

waveSize :: WaveFile -> Int32
waveSize waveFile = 36 + (dataSize waveFile)

dataSize :: WaveFile -> Int32
dataSize waveFile = fromInteger (toInteger (length (dataBytes waveFile)))

makeWaveFormatEx :: WaveFormatEx
makeWaveFormatEx = WaveFormatEx 1 2 44100 (ceiling (2 * 44100 * 2)) (2 * 2) 16;

writeWaveFile :: WaveFile -> FilePath -> IO ()
writeWaveFile waveFile filePath = do BSL.writeFile filePath (waveToByteString waveFile)

randomBytes :: [Word8]
randomBytes = BSL.unpack BSL.empty

encode32 i = BSL.pack  [(fromInteger ( toInteger ( shift i 0 ))),
                        (fromInteger ( toInteger  ( shift i 8 ))),
                        (fromInteger ( toInteger  ( shift i 16 ))),
                        (fromInteger ( toInteger  ( shift i 24 )))]
                        
encode16 i = BSL.pack [(fromInteger (toInteger (shift i 0))),
                        (fromInteger (toInteger (shift i 8)))]

waveFormatExToByteString :: WaveFormatEx -> BSL.ByteString
waveFormatExToByteString wfe = BSL.concat  [(encode16 (formatTag wfe)),
                                            (encode16 (channels wfe)),
                                            (encode32 (samplesPerSecond wfe)),
                                            (encode32 (avgBytesPerSecond wfe)),
                                            (encode16 (blockAlign wfe)),
                                            (encode16 (bitsPerSample wfe))]

waveToByteString :: WaveFile -> BSL.ByteString
waveToByteString waveFile = BSL.concat [(BSC.pack riffStr),
                                        (encode32 (waveSize waveFile)),
                                        (BSC.pack waveStr),
                                        (BSC.pack fmtStr),
                                        (encode32 waveFormatExSize),
                                        (waveFormatExToByteString (waveFormatEx waveFile)),
                                        (BSC.pack dataStr),
                                        (encode32 (dataSize waveFile)),
                                        (BSL.pack (dataBytes waveFile))]
