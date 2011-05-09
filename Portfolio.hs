module Portfolio where

import qualified Quote

data Lot = Lot {    pricePaid :: Double,
                    shares  :: Int  }
    
data Portfolio = Portfolio {    money :: Double,
                                lots :: [Lot]   }

commission :: Double
commission = 8.00

startingMoney :: Double
startingMoney = 10000.00
                                
value :: Double -> Portfolio -> Double
value curr p = foldl (value' curr) (money p) (lots p)

value' :: Double -> Double -> Lot -> Double
value' curr m l = m + (curr * (fromIntegral (shares l)))

buy :: Portfolio -> Quote.Quote -> Portfolio
buy s q =   let startingLots = lots s
                price = Quote.close q
                numShares = floor (((money s) - commission) / price)
                lot = Lot price numShares
            in  Portfolio ((money s) - ((price * (fromIntegral numShares)) + commission)) (lot:startingLots)
            
sell :: Portfolio -> Quote.Quote -> Portfolio
sell s q =  Portfolio ((value (Quote.close q) s) - commission) []
