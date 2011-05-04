module Trader where

trade :: [Quote] -> Int -> TradeState -> TradeState
trade qs idx state = trade' qs idx state (length qs)
 
trade' :: [Quote] -> Int -> TradeState -> Int -> TradeState
trade' qs idx state len = 
    if  idx >= (len-1)
    then    state
    else    trade' qs (idx+1) (buysell qs idx state) len

buysell :: [Quote] -> Int -> TradeState -> TradeState
buysell qs idx state = 
        let (buystate, bought) = (buy qs idx state)
        in  if  bought
            then    buystate
            else    let (sellstate, sold) = (sell qs idx state)
                in  if  sold
                    then    sellstate
                    else    state
 
buy :: [Quote] -> Int -> TradeState -> (TradeState, Bool)
buy qs idx state =
    let
        adjDown' = adjDown (seed state)
        adjDownMin' = assert (adjDownMin (seed state) >= 0) (adjDownMin (seed state))
        adjStagnant' = adjStagnant (seed state)
        adjStagnantMin' = assert (adjStagnantMin (seed state) >= 0) (adjStagnantMin (seed state))
        price' = assert (idx < length qs) (price (qs!!idx))
        adjDownMinIdx = assert (idx-adjDownMin' < length qs) (idx-adjDownMin')
        adjStagnantMinIdx = assert (idx-adjStagnantMin' < length qs) (idx-adjStagnantMin')
        cash' = cash state
        shares' = shares state
    in  if cash' >= (commission + price') && (shares' == 0)
        then if idx >= adjDownMin'
            then if price' <= ((price (qs!!adjDownMinIdx))-adjDown')
                then if idx >= adjStagnantMin'
                    then if (abs (price' - (price (qs!!adjStagnantMinIdx)))) <= adjStagnant'
                        then let
                            shares'' = (truncate ((cash' - commission)/price') :: Int)
                            pricePaid'' = price'
                            cash'' = cash' - ((pricePaid'' * (fromIntegral shares'')) + commission)
                        in
                            ((TradeState cash'' shares'' pricePaid'' (seed state)), True)
                        else (state, False)
                    else (state, False)
                else (state, False)
            else (state, False)
        else (state, False)
 
sell :: [Quote] -> Int -> TradeState -> (TradeState, Bool)
sell qs idx state =
    let
        adjPaid' = adjPaid (seed state)
        adjUp' = adjUp (seed state)
        adjUpMin' = adjUpMin (seed state)
        adjStagnant' = adjStagnant (seed state)
        adjStagnantMin' = adjStagnantMin (seed state)
        price' = assert (idx < length qs) (price (qs!!idx))
        cash' = cash state
        shares' = shares state
        pricePaid' = pricePaid state
    in
        if (shares' > 0) && (price' >= pricePaid' + adjPaid')
        then if (idx >= adjUpMin')
            then if (price' >= (price (qs!!(idx-adjUpMin'))) + adjUp')
                then if (idx >= adjStagnantMin')
                    then if (abs(price' - (price (qs!!(idx-adjStagnantMin'))))) <= adjStagnant'
                        then let
                            pricePaid'' = 0.0
                            cash'' = cash' + (price' * (fromIntegral shares')) - commission
                            shares'' = 0
                        in
                            ((TradeState cash'' shares'' pricePaid'' (seed state)), True)
                        else (state, False)
                    else (state, False)
                else (state, False)
            else (state, False)
        else (state, False)
