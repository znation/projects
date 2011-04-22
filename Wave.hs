module Wave (WaveFile(WaveFile, waveFormatEx, dataBytes), makeWave, writeWaveFile, fromHz, toByteString, fromByteString) where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Word
import Debugging
import Encoding
import System.IO
import qualified WaveFormatEx

samplesPerSecond = 44100
channels = 1

data WaveFile = WaveFile {  waveFormatEx        :: WaveFormatEx.WaveFormatEx,
                            dataBytes           :: [Word8] }
                            
instance Eq WaveFile where
    x == y = (waveFormatEx x == waveFormatEx y) && (dataBytes x == dataBytes y)
                            
makeWave :: WaveFile
makeWave = WaveFile WaveFormatEx.create (fromHz 440)

riffStr = "RIFF"
waveStr = "WAVE"
fmtStr = "fmt "
dataStr = "data"

waveSize :: WaveFile -> Int32
waveSize waveFile = 36 + (dataSize waveFile)

dataSize :: WaveFile -> Int32
dataSize waveFile = fromInteger (toInteger (length (dataBytes waveFile)))

writeWaveFile :: WaveFile -> FilePath -> IO ()
writeWaveFile waveFile filePath = do BSL.writeFile filePath (toByteString waveFile)

toByteString :: WaveFile -> BSL.ByteString
toByteString waveFile = BSL.concat [(BSC.pack riffStr),
                                        (BSL.pack (byteEncode (waveSize waveFile))),
                                        (BSC.pack waveStr),
                                        (BSC.pack fmtStr),
                                        (BSL.pack (byteEncode WaveFormatEx.size)),
                                        (WaveFormatEx.toByteString (waveFormatEx waveFile)),
                                        (BSC.pack dataStr),
                                        (BSL.pack (byteEncode (dataSize waveFile))),
                                        (BSL.pack (dataBytes waveFile))]
                                        
fromByteString :: BSL.ByteString -> WaveFile
fromByteString bs = let bytes = BSL.unpack bs
                        --riffStrBytes = take 4 bytes
                        --riffStr = BSC.unpack riffStrBytes
                        --waveSizeBytes = take 4 (drop 4 bytes)
                        --waveStrBytes = take 4 (drop 8 bytes)
                        --fmtStrBytes = take 4 (drop 12 bytes)
                        --waveFormatExSizeBytes = take 4 (drop 16 bytes)
                        waveFormatExBytes = take 16 (drop 20 bytes)
                        --dataStrBytes = take 4 (drop 36 bytes)
                        --dataSizeBytes = take 4 (drop 40 bytes)
                        dataBytes = drop 44 bytes
                    in  WaveFile (WaveFormatEx.fromBytes waveFormatExBytes) dataBytes

fromHz :: Int -> [Word8]
fromHz hz = fromHz' 0 44100 hz

fromHz' :: Int -> Int -> Int -> [Word8]
fromHz' idx max hz =    let numerator = toRational (hz * idx)
                            denominator = toRational (samplesPerSecond * channels)
                            weight = toRational ((maxBound::Int16) - 1)
                            value :: Int16
                            value = floor ((sin (pi * 2.0 * (fromRational (numerator / denominator)))) * (fromRational weight))
                            
                            makeBytes = byteEncode value
                        in  if (idx == max)
                            then    makeBytes
                            else    makeBytes ++ (fromHz' (idx+1) max hz)
