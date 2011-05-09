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
generate randomSeeds quotes =   let gen = take size randomSeeds
                                in  average (evaluate gen quotes)

average :: [Double] -> Double
average xs = (sum xs) / (fromIntegral (length xs))

evaluate :: Generation -> [Quote.Quote] -> [Double]
evaluate g qs = map (Trade.evaluate qs) g

cull :: Generation -> [Quote.Quote] -> [(Seed.Seed, Double)]
cull g qs = let results = evaluate g qs
                zipped = zip g results
            in  filter cull' zipped
            
cull' :: (Seed.Seed, Double) -> Bool
cull' (_,v) = v > Portfolio.startingMoney

refill :: Generation -> Generation -> Generation
refill g extras =   let l = length g
                    in  if      l > size
                        then    error "generation somehow got too big"
                        else    if      l == size
                                then    g
                                else    g ++ take (size - l) extras
