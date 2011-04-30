import Data.Int
import Data.Word
import Encoding
import qualified Test.QuickCheck as QC
import qualified Wave

main :: IO ()
main = do   check prop_importExportWave
            check prop_encodeDecode8
            check prop_encodeDecode8u
            check prop_encodeDecode16
            check prop_encodeDecode16u
            check prop_encodeDecode32
            check prop_encodeDecode32u

check :: QC.Testable a => a -> IO ()
check x = QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = 200}) x

prop_importExportWave :: Wave.WaveFile -> Bool
prop_importExportWave waveFile = waveFile == (Wave.fromByteString (Wave.toByteString waveFile))

prop_encodeDecode32 :: Int32 -> Bool
prop_encodeDecode32 x = x == (byteDecode (byteEncode (x)))

prop_encodeDecode32u :: Word32 -> Bool
prop_encodeDecode32u x = x == (byteDecode (byteEncode (x)))

prop_encodeDecode16 :: Int16 -> Bool
prop_encodeDecode16 x = x == (byteDecode (byteEncode (x)))

prop_encodeDecode16u :: Word16 -> Bool
prop_encodeDecode16u x = x == (byteDecode (byteEncode (x)))

prop_encodeDecode8 :: Int8 -> Bool
prop_encodeDecode8 x = x == (byteDecode (byteEncode (x)))

prop_encodeDecode8u :: Word8 -> Bool
prop_encodeDecode8u x = x == (byteDecode (byteEncode (x)))
