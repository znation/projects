module TradeState where

data TradeState = TradeState {cash :: Double,
                shares :: Int,
                pricePaid :: Double,
                seed :: Seed}
                deriving (Eq)
instance Ord TradeState where
    compare x y = compare (total x) (total y)
instance Show TradeState where
    show x = "Total: " ++ (show (total x)) ++ "\n" ++ (show (seed x)) ++ "\n"
instance QC.Arbitrary TradeState where
    arbitrary = do
                    a <- QC.arbitrary
                    b <- QC.arbitrary
                    c <- QC.arbitrary
                    d <- QC.arbitrary
                    return (TradeState a b c d)

total :: TradeState -> Double
total state = (cash state) + ((fromIntegral (shares state)) * (pricePaid state))

mutateSeed :: [Int] -> TradeState -> TradeState
mutateSeed randomInts s =
        let (d,i) = seedToLists (seed s)
            (randomInt1:randomInt2:_) = randomInts
            idx = randomInt1 `mod` 7 -- idx'th element of seed tuple
        in  if (idx < 4) -- idx 0-3 are Double, 4-6 are Int
            then
                let (xd, yd) = splitAt idx d
                    (y, yd') = splitAt 1 yd
                    y' = ((head y) - 0.20) + ((fromIntegral(randomInt2 `mod` 40))/100.0)
                in  TradeState (cash s) (shares s) (pricePaid s) (listsToSeed (xd ++ (y':yd')) i)
            else
                let (xi, yi) = splitAt (idx-4) i
                    (y, yi') = splitAt 1 yi
                    y' = max (((head y) - 20) + (randomInt2 `mod` 40)) 0
                in  TradeState (cash s) (shares s) (pricePaid s) (listsToSeed d (xi ++ (y':yi')))
