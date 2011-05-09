module Generation where

import qualified Quote
import qualified Seed

type Generation = [Seed.Seed]

size :: Int
size = 30

evaluate :: Generation -> [Quote.Quote] -> [[Seed.Action]]
evaluate g qs = map (Seed.evaluate qs) g
