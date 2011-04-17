import qualified Wave
import System.IO
                 
-- | 'main' runs the main program
main :: IO ()
main = do   Wave.writeWaveFile Wave.makeWave "output.wav"


