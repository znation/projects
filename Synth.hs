import System.IO
import qualified Wave

main :: IO ()
main = do   Wave.writeFile Wave.makeWave "output.wav"
