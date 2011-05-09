module Seed where

import qualified Quote
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
        return (normalize (Seed a b c d e))

normalize :: Seed -> Seed
normalize s =   let v = abs ((volume s) - fromInteger (floor (volume s)))
                    o = abs ((open s) - fromInteger (floor (open s)))
                    h = abs ((high s) - fromInteger (floor (high s)))
                    l = abs ((low s) - fromInteger (floor (low s)))
                    c = abs ((close s) - fromInteger (floor (close s)))
                in  Seed v o h l c

randoms :: Int -> IO [Seed]
randoms count = sequence (randoms' count [])

randoms' :: Int -> [IO Seed] -> [IO Seed]
randoms' 0 seeds = seeds
randoms' count seeds = randoms' (count - 1) (random:seeds)

random :: IO Seed
random = do
    a <- Rand.randomIO
    b <- Rand.randomIO
    c <- Rand.randomIO
    d <- Rand.randomIO
    e <- Rand.randomIO
    return (normalize (Seed a b c d e))
    
data Action = Buy | Sell
instance Show Action where
    show Buy = "Buy"
    show Sell = "Sell"

evaluate :: [Quote.Quote] -> Seed -> [Action]
evaluate qs s = let evaluate' _ _ = Buy
                in  map (evaluate' s) qs

prop_weightsBetweenZeroAndOne :: Seed -> Bool
prop_weightsBetweenZeroAndOne s =   let v = volume s
                                        o = open s
                                        h = high s
                                        l = low s
                                        c = close s
                                    in  (v >= 0.0 && v <= 1.0 && o >= 0.0 && o <= 1.0 && h >= 0.0 && h <= 1.0 && l >= 0.0 && l <= 1.0 && c >= 0.0 && c <= 1.0)
