module Problem031 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Utility

answer :: Int
answer = length combinations

result :: Integer
result = 200

type Solution = Map Integer Integer


insertOrUpdate :: (Ord a, Num b) => Map a b -> a -> b -> Map a b
insertOrUpdate s k v =  let trim :: (Ord a, Num b) => Map a b -> Map a b
                            trim m =    let predicate :: (Num c) => c -> Bool
                                            predicate x = x /= 0
                                            (r',_) = Map.partition predicate m
                                        in  r'

                            r = if      Map.member k s
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
       
combinations :: [Solution]
combinations =  let firstSolution = Map.fromList [(200,1)]
                    firstDenomination = 200
                in  combine firstSolution firstDenomination

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
            
combine :: Solution -> Integer -> [Solution]
combine s x =   let sum' = ssum s
                    (s',x') = removeHighestDenomination s
                    rest = combine s' x'
                in  if      lastSolution s
                    then    [s]
                    else    if      sum' < result
                            then    combine (insertOrUpdate s x 1) x
                            else    if      sum' > result
                                    then    rest
                                    else    s:rest
                
denominations :: [Integer]
denominations = reverse [1, 2, 5, 10, 20, 50, 100, 200]

