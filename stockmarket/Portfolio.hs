module Portfolio where

import Data.Int
import qualified Quote
import qualified Test.QuickCheck as QC

data Lot = Lot {    pricePaid :: Double,
                    shares  :: Int64  }
    deriving Show
    
instance QC.Arbitrary Lot where
    arbitrary = do
        p <- QC.arbitrary
        s <- QC.arbitrary
        return (Lot (abs p) (((abs s) `mod` 100000) + 1))
                    
data Portfolio = Portfolio {    money :: Double,
                                lots :: [Lot]   }
    deriving Show
    
instance QC.Arbitrary Portfolio where
    arbitrary = do
        m <- QC.arbitrary
        l <- QC.arbitrary
        return (Portfolio (abs m) l)

commission :: Double
commission = 8.00

startingMoney :: Double
startingMoney = 10000.00
                                
value :: Double -> Portfolio -> Double
value curr p = sum ((money p):(map (value' curr) (lots p)))

value' :: Double -> Lot -> Double
value' curr l = curr * (fromIntegral (shares l))

buy :: Portfolio -> Quote.Quote -> Portfolio
buy s q =   let startingLots = lots s
                price = Quote.close q
                numShares = floor (((money s) - commission) / price)
                lot = Lot price numShares
                newMoney = (money s) - ((price * (fromIntegral numShares)) + commission)
                newLots = lot:startingLots
            in  if      newMoney < 0
                then    error "Not enough money to buy"
                else    Portfolio newMoney newLots
            
sell :: Portfolio -> Quote.Quote -> Portfolio
sell s q =  let newMoney = (value (Quote.close q) s) - commission
                totalShares = sum (map shares (lots s))
            in  if      totalShares <= 0
                then    error "Not enough lots to sell"
                else    Portfolio newMoney []

-- TESTS
prop_sellReducesByCommission :: Portfolio -> Quote.Quote -> Bool
prop_sellReducesByCommission p q =  let originalValue = value (Quote.close q) p
                                        canSell = length (lots p) > 0
                                    in  if      canSell
                                        then    let soldValue = value (Quote.close q) (sell p q)
                                                    padding = 0.0001 -- padding to within a hundredth of a cent (exact values are susceptible to floating point rounding)
                                                    difference = originalValue - (soldValue + commission)
                                                in  padding > (abs difference)
                                        else    True

prop_buyReducesByCommission :: Portfolio -> Quote.Quote -> Bool
prop_buyReducesByCommission p q =   let originalValue = value (Quote.close q) p
                                        canBuy = ((money p) - commission) >= (Quote.close q)
                                    in  if      canBuy
                                        then    let boughtValue = value (Quote.close q) (buy p q)
                                                    padding = 0.0001
                                                    difference = originalValue - (boughtValue + commission)
                                                in  padding > (abs difference)
                                        else    True

