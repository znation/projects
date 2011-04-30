module Statistics (Statistics, generate, distribution) where

import qualified Data.Map as Map
import Data.Word

-- Map<Word8, Tuple<Map<Word8, Int>, Int>> where
-- the first Word8 is the current sample
-- the next Word8 is the next sample
-- the first Int is the number of occurrances of the sample pair
-- the second Int is the total number of occurrances with the first sample
type Statistics = Map.Map Word8 ((Map.Map Word8 Int), Int)
                    
add :: Statistics -> Word8 -> Word8 -> Statistics
add stats x y = if      (Map.member x stats)
                then    let inner = stats Map.! x
                            innerMap = fst inner
                            innerCount = snd inner
                        in  if      (Map.member y innerMap)
                            then    Map.insert x ((Map.insert y ((innerMap Map.! y) + 1) innerMap), innerCount+1) stats
                            else    Map.insert x ((Map.insert y 1 innerMap), innerCount+1) stats
                else    Map.insert x ((Map.insert y 1 Map.empty), 1) stats
                
generate :: [Word8] -> Statistics
generate = generate' Map.empty

generate' :: Statistics -> [Word8] -> Statistics
generate' stats (x:y:z:xs) = generate' (add stats x y) (y:z:xs)
generate' stats (x:y:[]) = add stats x y

-- generates a list of (key, frequency) tuples
percentages :: (Map.Map Word8 Int, Int) -> [(Word8, Double)]
percentages (map, total) = percentages' (map, total) (Map.keys map) []

percentages' :: (Map.Map Word8 Int, Int) -> [Word8] -> [(Word8, Double)] -> [(Word8, Double)]
percentages' _ [] frequencies = frequencies
percentages' (map, total) keys frequencies =    let key = head keys
                                                    count = map Map.! key
                                                    percentage = 100.0 * ((fromIntegral count) / (fromIntegral total))
                                                in  percentages' (map, total) (tail keys) ((key, percentage):frequencies)

-- generates a list of 100 items based on the distribution
distribution :: (Map.Map Word8 Int, Int) -> [Word8]
distribution (map, total) = distribution' (percentages (map, total)) []

distribution' :: [(Word8, Double)] -> [Word8] -> [Word8]
distribution' [] acc = acc
distribution' pcts acc =    let current = head pcts
                                key = fst current
                                freq = snd current
                            in  if (freq >= 1.0)
                                then distribution' ((key, freq-1.0):(tail pcts)) (key:acc)
                                else distribution' (tail pcts) (key:acc)
