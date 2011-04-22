import System.IO
import qualified Wave

main :: IO ()
main = do   Wave.writeWaveFile Wave.makeWave "output.wav"
