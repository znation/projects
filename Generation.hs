module Generation where

import qualified Quote
import qualified Seed
import qualified Trade

type Generation = [Seed.Seed]

size :: Int
size = 30

evaluate :: Generation -> [Quote.Quote] -> [Double]
evaluate g qs = map (Trade.evaluate qs) g
