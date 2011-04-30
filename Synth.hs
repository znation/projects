import System.IO
import qualified System.Random as Rand
import qualified Wave
import qualified Statistics
import qualified Data.Map as Map
import Data.Word

main :: IO ()
main = do   gen <- Rand.newStdGen
            input <- Wave.readFile "input.wav"
            let randoms = Rand.randoms gen :: [Double]
            Wave.writeFile (fromStatistics randoms input) "output.wav"
           
fromStatistics :: [Double] -> Wave.WaveFile -> Wave.WaveFile
fromStatistics randoms input =  let stats = Statistics.generate (Wave.dataBytes input)
                                    bytes = fromStatistics' randoms stats [] (length (Wave.dataBytes input))
                                in  Wave.fromData bytes

fromStatistics' :: [Double] -> Statistics.Statistics -> [Word8] -> Int -> [Word8]
fromStatistics' randoms stats bytes 0 = bytes
fromStatistics' randoms stats bytes i = let newByte = generateByte (randoms !! i) (stats Map.! (last bytes))
                                        in  fromStatistics' randoms stats (newByte : bytes) (i-1)

generateByte :: Double -> (Map.Map Word8 Int, Int) -> Word8
generateByte threshold (map, count) =   let dist = Statistics.distribution (map, count)
                                            idx = floor (threshold * 100.0)
                                        in  dist !! idx

