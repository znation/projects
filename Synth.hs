import System.IO
import qualified Wave
import qualified Statistics
import qualified Data.Map as Map
import Data.Word

main :: IO ()
main = do   Wave.writeFile (Wave.fromHz 440) "output.wav"

fromStatistics :: Wave.WaveFile -> Wave.WaveFile
fromStatistics input = let  stats = Statistics.generateStatistics (Wave.dataBytes input)
                            bytes = fromStatistics' stats (length (Wave.dataBytes input)) []
                       in   Wave.fromData bytes

fromStatistics' :: Statistics.Statistics -> Int -> [Word8] -> [Word8]
fromStatistics' stats i bytes = []