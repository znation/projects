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
                                    bitsPerSample       :: Word16}

data WaveFile = WaveFile {  riffStr             :: String,
                            size                :: Int32,
                            waveStr             :: String,
                            fmtStr              :: String,
                            waveFormatExSize    :: Int32,
                            waveFormatEx        :: WaveFormatEx,
                            dataStr             :: String,
                            dataLength          :: Int32,
                            dataBytes           :: [Word8]}
                 
-- | 'main' runs the main program
main :: IO ()
main = do   writeWaveFile makeWave "output.wav"

-- TODO: the 0 after RIFF should be the size of the rest of the file in bytes
-- TODO: the 0 after data should be the size of the data in bytes
makeWave :: WaveFile
makeWave = WaveFile "RIFF" 0 "WAVE" "fmt " 16 makeWaveFormatEx "data" 0 randomBytes

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
waveToByteString waveFile = BSL.concat [(BSC.pack (riffStr waveFile)),
                                        (encode32 (size waveFile)),
                                        (BSC.pack (waveStr waveFile)),
                                        (BSC.pack (fmtStr waveFile)),
                                        (encode32 (waveFormatExSize waveFile)),
                                        (waveFormatExToByteString (waveFormatEx waveFile)),
                                        (BSC.pack (dataStr waveFile)),
                                        (encode32 (dataLength waveFile)),
                                        (BSL.pack (dataBytes waveFile))]
                                       