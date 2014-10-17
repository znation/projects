module Sum where

data Sum = Sum [Integer]
    deriving Show

instance Eq Sum where
    Sum a == Sum b = countRepresentation a == countRepresentation b

countRepresentation :: [Integer] -> [Integer]
countRepresentation xs =    let highest :: Int
                                highest = fromIntegral (maximum xs)
                                empty = replicate highest 0
                            in  countRepresentation' xs empty

countRepresentation' :: [Integer] -> [Integer] -> [Integer]
countRepresentation' [] temp = temp
countRepresentation' (x:xs) temp =  let incremented = incrementAtIndex x temp
                                    in  countRepresentation' xs incremented

incrementAtIndex :: Integer -> [Integer] -> [Integer]
incrementAtIndex i xs = incrementAtIndex' i xs 0

incrementAtIndex' :: Integer -> [Integer] -> Integer -> [Integer]
incrementAtIndex' _ [] _ = []
incrementAtIndex' i (x:xs) c =  if      i == c
                                then    (x+1):(incrementAtIndex' i xs (c+1))
                                else    x:(incrementAtIndex' i xs (c+1))

toSum :: [Integer] -> Sum
toSum xs = Sum xs

add :: Integer -> Sum -> Sum
add x (Sum values) =    let xs :: [Integer]
                            xs = values
                            added = x:xs
                        in  toSum added

