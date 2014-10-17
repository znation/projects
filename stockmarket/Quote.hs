module Quote where

import Data.Int
import qualified Test.QuickCheck as QC

data Quote = Quote {symbol :: String,
                    day :: String,
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
        h <- QC.arbitrary
        return (Quote a b (abs c) (abs d) (abs e) (abs f) (abs g) (abs h))
        
makeQuote :: Int -> [String] -> Quote
makeQuote idx (a:b:c:d:e:f:g:h:[]) =    let ratio = (read h::Double) / (read f::Double)
                                            adjOpen = (read c::Double) * ratio
                                            adjHigh = (read d::Double) * ratio
                                            adjLow = (read e::Double) * ratio
                                            adjClose = read h::Double -- also e * ratio
                                            vol = read g::Int64
                                        in  Quote a b idx vol adjOpen adjHigh adjLow adjClose
makeQuote _ _ = error "Insufficient data to make a quote"

makeQuotes :: [[String]] -> [Quote]
makeQuotes = makeQuotes' 0

makeQuotes' :: Int -> [[String]] -> [Quote]
makeQuotes' idx (x:xs) = (makeQuote idx x):(makeQuotes' (idx + 1) xs)
makeQuotes' _ [] = []
