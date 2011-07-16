module Problem031 where

-- TODO -- this is doing permutations, not combinations

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MemoCombinators as Memo
import Utility

answer :: Int
answer = length combinations2

result :: Integer
result = 200

type Solution = Map Integer Integer

trim :: (Ord a, Num b) => Map a b -> Map a b
trim m =    let predicate :: (Num c) => c -> Bool
                predicate x = x /= 0
                (r,_) = Map.partition predicate m
            in  r

insertOrUpdate :: (Ord a, Num b) => Map a b -> a -> b -> Map a b
insertOrUpdate s k v =  let r = if      Map.member k s
                                then    Map.adjust (+v) k s
                                else    Map.insert k v s
                        in  trim r

ssum :: Solution -> Integer
ssum s =    let ssum' :: (Integer,Integer) -> Integer
                ssum' (k,v) = k * v
            in  sum (map ssum' (Map.toList s))

valid :: Solution -> Bool
valid s =   let sum' = (ssum s) `debug` ("Validating " ++ (show s))
            in  sum' == result

type Range = [Integer]

makeSolution :: Range -> Solution
makeSolution r = Map.fromList (zip denominations r)
       
combinations2 :: [Solution]
-- combinations2 = let ranges = [[a,b,c,d,e,f,g,h] | a <- [0..1], b <- [0..2], c <- [0..4], d <- [0..10], e <- [0..20], f <- [0..40], g <- [0..100], h <- [0..200]]
                    -- solutions = map makeSolution ranges
                -- in  filter valid solutions
combinations2 = let firstSolution = Map.fromList [(200,1)]
                    firstDenomination = 200
                in  combine2 firstSolution firstDenomination

lowerDenomination :: Integer -> Integer
lowerDenomination 200 = 100
lowerDenomination 100 = 50
lowerDenomination 50 = 20
lowerDenomination 20 = 10
lowerDenomination 10 = 5
lowerDenomination 5 = 2
lowerDenomination 2 = 1
lowerDenomination x = error ("Can't lower denomination " ++ (show x))

lastSolution :: Solution -> Bool
lastSolution s =    let lastSolution' :: Maybe Integer -> Bool
                        lastSolution' Nothing = False
                        lastSolution' (Just x) = x == result
                    in  (lastSolution' (Map.lookup 1 s)) && (ssum s == result)

removeHighestDenomination :: Solution -> (Solution, Integer)
removeHighestDenomination s =   let ks = reverse (sort (Map.keys s))
                                    k = head ks
                                    l = lowerDenomination k
                                in  ((insertOrUpdate s k (-1)),l)
            
combine2 :: Solution -> Integer -> [Solution]
combine2 s x =  let sum' = ssum s
                    (s',x') = removeHighestDenomination s
                    rest = combine2 s' x'
                in  if      lastSolution s
                    then    [s]
                    else    if      sum' < result
                            then    combine2 (insertOrUpdate s x 1) x
                            else    if      sum' > result
                                    then    rest
                                    else    s:rest
                
denominations :: [Integer]
denominations = reverse [1, 2, 5, 10, 20, 50, 100, 200]

combinations :: Integer
combinations = sum (map firstCombine denominations)

firstCombine :: Integer -> Integer
firstCombine x =    let y = x `debug` "Starting with"
                    in  combine 0 y

combine :: Integer -> Integer -> Integer
combine = Memo.memo2 Memo.integral Memo.integral combine'

-- returns combinations that add up to 2 pounds
combine' :: Integer -> Integer -> Integer
combine' acc x =    let total = acc + x
                        diff = result - total
                        ds = filter (<= diff) denominations
                    in  if      diff == 0
                        then    1
                        else    if      diff < 0
                                then    0
                                else    sum (map (combine total) ds)

