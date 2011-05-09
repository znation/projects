module Quote where

import Data.Int
import qualified Test.QuickCheck as QC

data Quote = Quote {day :: String,
                    index :: Int,
                    volume :: Int64,
                    open, high, low, close :: Double}
            deriving (Show)

instance QC.Arbitrary Quote where
    arbitrary = do
        a <- QC.arbitrary
        b <- QC.arbitrary
        c <- QC.arbitrary
        d <- QC.arbitrary
        e <- QC.arbitrary
        f <- QC.arbitrary
        g <- QC.arbitrary
        return (Quote a (abs b) (abs c) (abs d) (abs e) (abs f) (abs g))
        
makeQuote :: Int -> [String] -> Quote
makeQuote idx (a:b:c:d:e:f:g:[]) =  let ratio = (read g::Double) / (read e::Double)
                                        adjOpen = (read b::Double) * ratio
                                        adjHigh = (read c::Double) * ratio
                                        adjLow = (read d::Double) * ratio
                                        adjClose = read g::Double -- also e * ratio
                                        vol = read f::Int64
                                    in  Quote a idx vol adjOpen adjHigh adjLow adjClose --(read y::Double)
makeQuote _ _ = error "Insufficient data to make a quote"

makeQuotes :: [[String]] -> [Quote]
makeQuotes = makeQuotes' 0

makeQuotes' :: Int -> [[String]] -> [Quote]
makeQuotes' idx (x:xs) = (makeQuote idx x):(makeQuotes' (idx + 1) xs)
makeQuotes' _ [] = []
