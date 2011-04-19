module Wave (makeWave, writeWaveFile, fromHz) where

import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

import Data.Int
import Data.Word
import Encoding
import qualified WaveFormatEx
import System.IO

data WaveFile = WaveFile {  waveFormatEx        :: WaveFormatEx.WaveFormatEx,
                            dataBytes           :: [Word8] }
                            
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
writeWaveFile waveFile filePath = do BSL.writeFile filePath (waveToByteString waveFile)

randomBytes :: [Word8]
randomBytes = BSL.unpack BSL.empty

waveToByteString :: WaveFile -> BSL.ByteString
waveToByteString waveFile = BSL.concat [(BSC.pack riffStr),
                                        (BSL.pack (byteEncode (waveSize waveFile))),
                                        (BSC.pack waveStr),
                                        (BSC.pack fmtStr),
                                        (BSL.pack (byteEncode WaveFormatEx.size)),
                                        (WaveFormatEx.toByteString (waveFormatEx waveFile)),
                                        (BSC.pack dataStr),
                                        (BSL.pack (byteEncode (dataSize waveFile))),
                                        (BSL.pack (dataBytes waveFile))]

fromHz :: Int -> [Word8]
fromHz hz = fromHz' 0 44100 hz

fromHz' :: Int -> Int -> Int -> [Word8]
fromHz' idx max hz =    let numerator :: Double
                            numerator = fromInteger (toInteger (hz * idx))
                            denominator = 44100.0 * 2.0
                            makeBytes = byteEncode (sin (pi * 2.0 * (numerator / denominator)))
                        in  if (idx == max)
                            then    makeBytes
                            else    makeBytes ++ (fromHz' (idx+1) max hz)
