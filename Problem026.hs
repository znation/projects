module Problem026 where

answer :: Int
answer = longestCycle [2..1000]

maxLength :: Int
maxLength = 30

longestCycle :: [Int] -> Int
longestCycle =  let longestCycle' :: Int -> Int -> [Int] -> Int
                    longestCycle' d _ [] = d
                    longestCycle' d l (x:xs) =  let m = cycleLength x
                                                in  if      m > l
                                                    then    longestCycle' x m xs
                                                    else    longestCycle' d l xs
                in  longestCycle' 0 0

-- What's the cycle length of 1/d?                                                    
cycleLength :: Int -> Int
cycleLength d = let xs = (filter (cycleLength' d 0) [1..maxLength])
                in  if      length xs == 0
                    then    0
                    else    head xs
                    
-- Is there a cycle of length l starting at position p in (1/d)?
cycleLength' :: Int -> Int -> Int -> Bool
cycleLength' d p l =    if      p == maxLength
                        then    False
                        else    let frac = drop (1 + p) (longDivision 1 d)
                                    section1 = take l frac
                                    section2 = take l (drop l frac)
                                    ls1 = length section1
                                    ls2 = length section2
                                in  if      ls1 == l && ls1 == ls2 && section1 == section2
                                    then    True
                                    else    cycleLength' d (p+1) l

-- performs long division (x / y)
longDivision :: Int -> Int -> [Int]
longDivision 0 _ = []
longDivision x y =  let (q,r) = x `quotRem` y
                    in  if      q == 0
                        then    0:(longDivision (x * 10) y)
                        else    q:(longDivision (r * 10) y)
