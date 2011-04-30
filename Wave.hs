module Wave (   WaveFile(WaveFile, waveFormatEx, dataBytes),
                Wave.readFile,
                Wave.writeFile,
                fromData,
                fromHz,
                toByteString,
                fromByteString) where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Word
import Debugging
import Encoding
import System.IO
import qualified WaveFormatEx

data WaveFile = WaveFile {  waveFormatEx        :: WaveFormatEx.WaveFormatEx,
                            dataBytes           :: [Word8] }
                            
instance Eq WaveFile where
    x == y = (waveFormatEx x == waveFormatEx y) && (dataBytes x == dataBytes y)

riffStr = "RIFF"
waveStr = "WAVE"
fmtStr = "fmt "
dataStr = "data"

fromData :: [Word8] -> WaveFile
fromData bytes = WaveFile WaveFormatEx.create bytes

waveSize :: WaveFile -> Int32
waveSize waveFile = 36 + (dataSize waveFile)

dataSize :: WaveFile -> Int32
dataSize waveFile = fromInteger (toInteger (length (dataBytes waveFile)))

writeFile :: WaveFile -> FilePath -> IO ()
writeFile waveFile filePath = do BSL.writeFile filePath (toByteString waveFile)

readFile :: FilePath -> IO WaveFile
readFile filePath = do  bytes <- (BSL.readFile filePath)
                        return (fromByteString bytes)

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

fromHz :: Int -> WaveFile
fromHz hz = WaveFile WaveFormatEx.create (fromHz' 0 44100 hz)

fromHz' :: Int -> Int -> Int -> [Word8]
fromHz' idx max hz =    let samplesPerSecond = WaveFormatEx.samplesPerSecond WaveFormatEx.create
                            channels = WaveFormatEx.channels WaveFormatEx.create
                            numerator = toRational (hz * idx)
                            denominator = toRational (fromIntegral samplesPerSecond * fromIntegral channels)
                            weight = toRational ((maxBound::Int16) - 1)
                            value :: Int16
                            value = floor ((sin (pi * 2.0 * (fromRational (numerator / denominator)))) * (fromRational weight))
                            
                            makeBytes = byteEncode value
                        in  if (idx == max)
                            then    makeBytes
                            else    makeBytes ++ (fromHz' (idx+1) max hz)
