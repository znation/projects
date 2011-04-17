module Wave (makeWave, writeWaveFile) where

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
makeWave = WaveFile WaveFormatEx.create randomBytes

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
                                        (encode32 (waveSize waveFile)),
                                        (BSC.pack waveStr),
                                        (BSC.pack fmtStr),
                                        (encode32 WaveFormatEx.size),
                                        (WaveFormatEx.toByteString (waveFormatEx waveFile)),
                                        (BSC.pack dataStr),
                                        (encode32 (dataSize waveFile)),
                                        (BSL.pack (dataBytes waveFile))]
