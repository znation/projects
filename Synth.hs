import System.IO
import qualified Wave
import qualified Statistics
import qualified Data.Map as Map
import Data.Word

main :: IO ()
main = do   input <- Wave.readFile "input.wav"
            Wave.writeFile (fromStatistics input) "output.wav"

fromStatistics :: Wave.WaveFile -> Wave.WaveFile
fromStatistics input = let  stats = Statistics.generate (Wave.dataBytes input)
                            bytes = fromStatistics' stats [] (length (Wave.dataBytes input))
                       in   Wave.fromData bytes

fromStatistics' :: Statistics.Statistics -> [Word8] -> Int -> [Word8]
fromStatistics' stats bytes 0 = bytes
fromStatistics' stats bytes i = let newByte = 0
                                in  fromStatistics' stats (newByte : bytes) (i-1)
                                
