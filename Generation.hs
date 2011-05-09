module Generation where

import qualified Portfolio
import qualified Quote
import qualified Seed
import qualified Trade

type Generation = [Seed.Seed]

size :: Int -- The number of seeds per generation
size = 30

count :: Int -- The number of generations to run
count = 100

generate :: Generation -> [Quote.Quote] -> Double
generate randomSeeds quotes = generate' count (drop size randomSeeds) (take size randomSeeds) quotes

generate' :: Int -> Generation -> Generation -> [Quote.Quote] -> Double
generate' 0 _ gen quotes = average (evaluate gen quotes)
generate' c randomSeeds gen quotes =    let culled = cull gen quotes
                                            refilled = refill culled (take count randomSeeds)
                                        in  generate' (c - 1) (drop count randomSeeds) refilled quotes
                                
average :: [Double] -> Double
average xs = (sum xs) / (fromIntegral (length xs))

evaluate :: Generation -> [Quote.Quote] -> [Double]
evaluate g qs = map (Trade.evaluate qs) g

cull :: Generation -> [Quote.Quote] -> Generation
cull g qs = let results = evaluate g qs
                zipped = zip g results
                filtered = filter cull' zipped
            in  fst (unzip (filtered))
            
cull' :: (Seed.Seed, Double) -> Bool
cull' (_,v) = v > Portfolio.startingMoney

refill :: Generation -> Generation -> Generation
refill g extras =   let l = length g
                    in  if      l > size
                        then    error "generation somehow got too big"
                        else    if      l == size
                                then    g
                                else    g ++ take (size - l) extras
