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
