import qualified Test.QuickCheck as QC

main :: IO ()
main = do
    QC.quickCheck (prop_randomSeedReallyRandom randomInts)
    QC.quickCheck (prop_randomSeedNotReallyRandom randomInts)
    QC.quickCheck (prop_generateMultipliesByTwo randomInts)
    QC.quickCheck (prop_bestPreservesOrdering states)

-- TESTS
prop_randomSeedReallyRandom :: [Int] -> Int -> Bool
prop_randomSeedReallyRandom randomInts idx = (randomSeed (drop idx randomInts) /= randomSeed (drop ((abs idx)+1) randomInts))
prop_randomSeedNotReallyRandom :: [Int] -> Int -> Bool
prop_randomSeedNotReallyRandom randomInts idx = (randomSeed (drop idx randomInts) == randomSeed (drop idx randomInts))
prop_generateMultipliesByTwo :: [Int] -> [TradeState] -> Bool
prop_generateMultipliesByTwo randomInts states = ((length (generate states randomInts)) == 2 * (length states))
prop_bestPreservesOrdering :: [TradeState] -> [Quote] -> Int -> Bool
prop_bestPreservesOrdering states qs n = (best (n `mod` 20) states qs) == (take (n `mod` 20) (best 20 states qs))