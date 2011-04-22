import qualified Test.QuickCheck as QC
import qualified Wave
import qualified WaveFormatEx

main :: IO ()
main = do   QC.quickCheck prop_importExport

prop_importExport waveFile = waveFile == (Wave.fromByteString (Wave.toByteString waveFile))

instance QC.Arbitrary Wave.WaveFile where
    arbitrary = do  waveFormatEx <- QC.arbitrary
                    dataBytes <- QC.arbitrary
                    return (Wave.WaveFile waveFormatEx dataBytes)

instance Show Wave.WaveFile where
    show waveFile = "[WaveFileInstance]"

instance QC.Arbitrary WaveFormatEx.WaveFormatEx where
    arbitrary = do  formatTag <- QC.arbitrary
                    channels <- QC.arbitrary
                    samplesPerSecond <- QC.arbitrary
                    avgBytesPerSecond <- QC.arbitrary
                    blockAlign <- QC.arbitrary
                    bitsPerSample <- QC.arbitrary
                    return (WaveFormatEx.WaveFormatEx formatTag channels samplesPerSecond avgBytesPerSecond blockAlign bitsPerSample)
