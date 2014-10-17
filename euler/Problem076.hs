module Problem076 where

import qualified Data.List as List
import Sum

answer :: Int
answer = length (newSums 100)

unpair :: (a,a) -> [a]
unpair (x,y) = [x,y]

countUniqueSums :: Integer -> Int
countUniqueSums x = (length (uniqueSums x))

uniqueSums :: Integer -> [Sum]
uniqueSums x = List.nub (sums x)

sums :: Integer -> [Sum]
sums x =    let a :: Integer
                a = x - 1
                b :: [Integer]
                b = [1..a]
                c :: [Integer]
                c = reverse b
                d :: [(Integer, Integer)]
                d = zip b c
                e :: [[Integer]]
                e = map unpair d
                f = map toSum e
            in  f ++ (concat (map sums' e))

sums' :: [Integer] -> [Sum]
sums' s =   let a = s !! 0
                b = s !! 1
            in  (map (Sum.add a) (sums b))

newSums :: Integer -> [Sum]
newSums x = newSums' x [(toSum [(x-1),1])]

newSums' :: Integer -> [Sum] -> [Sum]
newSums' x temp =   let prevSum = head temp
                    in  if     allOnes x prevSum
                        then   temp
                        else   newSums' x ((nextSum x prevSum):temp)

allOnes :: Integer -> Sum -> Bool
allOnes x (Sum components) = components == replicate (fromIntegral x) 1

nextSum :: Integer -> Sum -> Sum
nextSum x prevSum = let (idx,val) = lastNonOneValue prevSum
                        Sum components = prevSum
                    in  if      (idx == 0) 
                        then    toSum (constructSum x (val-1) [])
                        else    toSum (constructSum x (val-1) (take idx components))

constructSum :: Integer -> Integer -> [Integer] -> [Integer]
constructSum x maxValue inProgress =    let total = sum inProgress --`debug` "Total so far"
                                            difference = x - total --`debug` "Difference"
                                            nextVal = min difference maxValue --`debug` "Next value"
                                        in  if      total == x
                                            then    inProgress --`debug` "Constructed sum"
                                            else    constructSum x maxValue (inProgress ++ [nextVal])

lastNonOneValue :: Sum -> (Int, Integer)
lastNonOneValue (Sum components) =  let zipped = zip [0..] components
                                        reversed = reverse zipped
                                        filtered = dropWhile (\(_,val) -> val == 1) reversed
                                    in  head filtered

