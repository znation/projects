import qualified Portfolio
import qualified Seed
import qualified Test.QuickCheck as QC

main :: IO ()
main = do
    check Seed.prop_weightsBetweenZeroAndOne
    check Portfolio.prop_sellReducesByCommission
    check Portfolio.prop_buyReducesByCommission

check :: QC.Testable a => a -> IO ()
check x = QC.quickCheckWith (QC.stdArgs {QC.maxSuccess = 1000}) x
