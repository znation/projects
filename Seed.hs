module Seed where

data Seed = Seed{adjDown, adjStagnant, adjUp, adjPaid :: Double,
        adjDownMin, adjStagnantMin, adjUpMin :: Int}
            deriving (Show, Eq)

instance QC.Arbitrary Seed where
    arbitrary = do
        a <- QC.arbitrary
        b <- QC.arbitrary
        c <- QC.arbitrary
        d <- QC.arbitrary
        e <- QC.arbitrary
        f <- QC.arbitrary
        g <- QC.arbitrary
        return (Seed a b c d (abs e) (abs f) (abs g))

-- listsToSeed takes a list of Doubles and a list of Ints and makes it into a Seed
listsToSeed :: [Double] -> [Int] -> Seed
listsToSeed (a:b:c:d:_) (e:f:g:_) = Seed a b c d e f g
listsToSeed _ _ = error "Don't know how to make a seed out of random crap."
 
seedToLists :: Seed -> ([Double], [Int])
seedToLists s =
        let a = adjDown s
            b = adjStagnant s
            c = adjUp s
            d = adjPaid s
            e = adjDownMin s
            f = adjStagnantMin s
            g = adjUpMin s
        in  ((a:b:c:d:[]),(e:f:g:[]))

randomSeed :: [Int] -> Seed
randomSeed (r1:r2:r3:r4:r5:r6:r7:_) = 
        let stagmin = r6 `mod` 5
        in  Seed
                    ((fromIntegral(r1 `mod` 10))/100)
                    ((fromIntegral(r2 `mod` 5))/100)
                    ((fromIntegral(r3 `mod` 10))/100)
                    ((fromIntegral(r4 `mod` 5))/100)
                    ((r5 `mod` 5) + stagmin)
                    stagmin
                    ((r7 `mod` 5) + stagmin)
randomSeed _ = error "Can't make a random seed with less than 7 random numbers, idiot!"

randomSeeds :: Int -> [Int] -> [Seed]
randomSeeds 0 _ = []
randomSeeds n randomInts = (randomSeed (take 7 randomInts)):(randomSeeds (n-1) (drop 7 randomInts))
