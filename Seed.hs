module Seed where

import qualified Random as Rand
import qualified System.IO.Unsafe as Unsafe
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
        
randomSeeds :: Int -> [Seed]
randomSeeds count = randomSeeds' count []

randomSeeds' :: Int -> [Seed] -> [Seed]
randomSeeds' 0 seeds = seeds
randomSeeds' count seeds = randomSeeds' (count - 1) (randomSeed:seeds)

-- TODO -- always returns the same seed
randomSeed :: Seed
randomSeed =    let a = Unsafe.unsafePerformIO Rand.randomIO
                    b = Unsafe.unsafePerformIO Rand.randomIO
                    c = Unsafe.unsafePerformIO Rand.randomIO
                    d = Unsafe.unsafePerformIO Rand.randomIO
                    e = Unsafe.unsafePerformIO Rand.randomIO
                in  Seed a b c d e
