module Portfolio where

import qualified Quote
import qualified Test.QuickCheck as QC

data Lot = Lot {    pricePaid :: Double,
                    shares  :: Int  }
    deriving Show
    
instance QC.Arbitrary Lot where
    arbitrary = do
        p <- QC.arbitrary
        s <- QC.arbitrary
        return (Lot (abs p) ((abs s)+1))
                    
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
            in  Portfolio ((money s) - ((price * (fromIntegral numShares)) + commission)) (lot:startingLots)
            
sell :: Portfolio -> Quote.Quote -> Portfolio
sell s q =  Portfolio ((value (Quote.close q) s) - commission) []

prop_sellReducesByCommission :: Portfolio -> Quote.Quote -> Bool
prop_sellReducesByCommission p q =  let originalValue = value (Quote.close q) p
                                        canSell = length (lots p) > 0
                                        soldValue = value (Quote.close q) (sell p q)
                                        
                                    in  if      canSell
                                        then    let padding = 0.0001 -- padding to within a hundredth of a cent (exact values are susceptible to floating point rounding)
                                                    difference = originalValue - (soldValue + commission)
                                                in  padding > (abs difference)
                                        else    True
