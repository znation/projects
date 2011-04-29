module Statistics where

import qualified Data.Map as Map
import Data.Word

type Statistics = Map.Map Word8 (Map.Map Word8 Int)
                    
add :: Statistics -> Word8 -> Word8 -> Statistics
add stats x y = if      (Map.member x stats)
                then    let inner = stats Map.! x
                        in  if      (Map.member y inner)
                            then    Map.insert x (Map.insert y ((inner Map.! y) + 1) inner) stats
                            else    Map.insert x (Map.insert y 1 inner) stats
                else    Map.insert x (Map.insert y 1 Map.empty) stats
                
generateStatistics :: [Word8] -> Statistics
generateStatistics = generateStatistics' Map.empty

generateStatistics' :: Statistics -> [Word8] -> Statistics
generateStatistics' stats (x:y:z:xs) = generateStatistics' (add stats x y) (y:z:xs)
generateStatistics' stats (x:y:[]) = add stats x y
