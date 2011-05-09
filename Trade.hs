module Trade where

import qualified Quote
import qualified Seed

commission :: Double
commission = 8.00

data Action = Buy | Sell
    deriving Eq

instance Show Action where
    show Buy = "Buy"
    show Sell = "Sell"
    
data State = State {    money :: Double,
                        shares :: Int   }

buysell :: Seed.Seed -> Quote.Quote -> Action
buysell s q =   if      Seed.open s * Quote.open q > Seed.close s * Quote.close q
                then    Buy
                else    Sell

evaluate :: [Quote.Quote] -> Seed.Seed -> Double
evaluate qs s = let actions = map (buysell s) qs -- [Action]
                    zipped = zip qs actions
                    startingState = State 10000.00 0
                    tradedState = foldl trade startingState zipped
                in  value tradedState

trade :: State -> (Quote.Quote, Action) -> State
trade s (q, a) =    let startingMoney = money s
                        startingShares = shares s
                    in  if      (startingMoney - commission > Quote.close q) && (a == Buy)
                        then    buy s q
                        else    if      (startingShares > 0) && (a == Sell)
                                then    sell s q
                                else    s

buy :: State -> Quote.Quote -> State
buy s q =   let startingMoney = money s
                startingShares = shares s
                price = Quote.close q
                numShares = floor ((startingMoney - commission) / price)
            in  State (startingMoney - ((price * (fromIntegral numShares)) + commission)) (startingShares + numShares)
            
sell :: State -> Quote.Quote -> State
sell s q =  let startingMoney = money s
                startingShares = shares s
                price = Quote.close q
            in  State (startingMoney + (price * (fromIntegral startingShares)) - commission) 0

value :: State -> Double
value _ = error "TODO implement value"
