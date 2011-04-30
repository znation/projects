module Statistics (Statistics, generate) where

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
                
generate :: [Word8] -> Statistics
generate = generate' Map.empty

generate' :: Statistics -> [Word8] -> Statistics
generate' stats (x:y:z:xs) = generate' (add stats x y) (y:z:xs)
generate' stats (x:y:[]) = add stats x y
