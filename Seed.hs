module Seed where

import qualified Random as Rand
import qualified Test.QuickCheck as QC

data Seed = Seed {volume, open, high, low, close :: Double}
    deriving Show

instance QC.Arbitrary Seed where
    arbitrary = do
        a <- QC.arbitrary
        b <- QC.arbitrary
        c <- QC.arbitrary
        d <- QC.arbitrary
        e <- QC.arbitrary
        return (Seed a b c d e)
        
randomSeeds :: Int -> IO [Seed]
randomSeeds count = sequence (randomSeeds' count [])

randomSeeds' :: Int -> [IO Seed] -> [IO Seed]
randomSeeds' 0 seeds = seeds
randomSeeds' count seeds = randomSeeds' (count - 1) (randomSeed:seeds)

randomSeed :: IO Seed
randomSeed = do
    a <- Rand.randomIO
    b <- Rand.randomIO
    c <- Rand.randomIO
    d <- Rand.randomIO
    e <- Rand.randomIO
    return (Seed a b c d e)
