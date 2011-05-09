module Trade where

import qualified Portfolio
import qualified Quote
import qualified Seed

data Action = Buy | Sell
    deriving Eq

instance Show Action where
    show Buy = "Buy"
    show Sell = "Sell"

buysell :: Seed.Seed -> Quote.Quote -> Action
buysell s q =   if      Seed.open s * Quote.open q > Seed.close s * Quote.close q
                then    Buy
                else    Sell

evaluate :: [Quote.Quote] -> Seed.Seed -> Double
evaluate qs s = let actions = map (buysell s) qs -- [Action]
                    zipped = zip qs actions
                    startingState = Portfolio.Portfolio 10000.00 []
                    tradedState = foldl trade startingState zipped
                in  Portfolio.value (Quote.close (last qs)) tradedState

trade :: Portfolio.Portfolio -> (Quote.Quote, Action) -> Portfolio.Portfolio
trade s (q, a) =    let startingMoney = Portfolio.money s
                        startingLots = Portfolio.lots s
                    in  if      (startingMoney - Portfolio.commission > Quote.close q) && (a == Buy)
                        then    Portfolio.buy s q
                        else    if      ((length startingLots) > 0) && (a == Sell)
                                then    Portfolio.sell s q
                                else    s

