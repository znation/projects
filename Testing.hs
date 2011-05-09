import qualified Seed
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
    check Seed.prop_weightsBetweenZeroAndOne

check :: QC.Testable a => a -> IO ()
check x = QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = 200}) x
