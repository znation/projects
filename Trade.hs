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
    
data Lot = Lot {    pricePaid :: Double,
                    shares  :: Int  }
    
data Portfolio = Portfolio {    money :: Double,
                                lots :: [Lot]   }

buysell :: Seed.Seed -> Quote.Quote -> Action
buysell s q =   if      Seed.open s * Quote.open q > Seed.close s * Quote.close q
                then    Buy
                else    Sell

evaluate :: [Quote.Quote] -> Seed.Seed -> Double
evaluate qs s = let actions = map (buysell s) qs -- [Action]
                    zipped = zip qs actions
                    startingState = Portfolio 10000.00 []
                    tradedState = foldl trade startingState zipped
                in  value (Quote.close (last qs)) tradedState

trade :: Portfolio -> (Quote.Quote, Action) -> Portfolio
trade s (q, a) =    let startingMoney = money s
                        startingLots = lots s
                    in  if      (startingMoney - commission > Quote.close q) && (a == Buy)
                        then    buy s q
                        else    if      ((length startingLots) > 0) && (a == Sell)
                                then    sell s q
                                else    s

buy :: Portfolio -> Quote.Quote -> Portfolio
buy s q =   let startingMoney = money s
                startingLots = lots s
                price = Quote.close q
                numShares = floor ((startingMoney - commission) / price)
                lot = Lot price numShares
            in  Portfolio (startingMoney - ((price * (fromIntegral numShares)) + commission)) (lot:startingLots)
            
sell :: Portfolio -> Quote.Quote -> Portfolio
sell s q =  Portfolio ((value (Quote.close q) s) - commission) []

value :: Double -> Portfolio -> Double
value curr p = foldl (value' curr) (money p) (lots p)

value' :: Double -> Double -> Lot -> Double
value' curr m l = m + (curr * (fromIntegral (shares l)))
