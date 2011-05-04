module Quote where

data Quote = Quote {day :: Date,
                    index :: Int32,
                    volume :: Int64,
                    open, high, low, close :: Double}
            deriving (Show)

instance QC.Arbitrary Quote where
    arbitrary = do
        a <- QC.arbitrary
        return (Quote (abs a))

makeQuote :: [String] -> Quote
makeQuote (_:y:_) = Quote (read y::Double)
makeQuote _ = error "Insufficient data to make a quote"

makeQuotes :: [[String]] -> [Quote]
makeQuotes (x:xs) = (makeQuote x):(makeQuotes xs)
makeQuotes [] = []
